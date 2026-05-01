use abap_ast::SyntaxKind;
use abap_ast::arena::{NodeId, SyntaxTreeBuilder};
use abap_lexer::{Token, TokenKind};

use crate::block_helpers::{
    Boundary, error_token_children, inline_name_spacing_is_valid, is_keyword,
    next_after_unterminated_scan, parse_body_until_keywords, parse_end_keyword,
    parse_header_until_period, parse_inline_name, scan_boundary_keywords, skip_trivia,
};
use crate::expr::{parse_arithmetic_expr, parse_logical_expr};
use crate::stmt_period::{
    StmtPeriodScan, delimiter_error, has_non_comment_tokens, scan_until_statement_period,
    unterminated_err_end,
};
use crate::syntax::token_leaf;
use crate::type_ref::build_type_ref_node;

fn scan_catch_type_ref_end(tokens: &[Token], idx: usize) -> usize {
    let Some(first) = tokens.get(idx) else {
        return idx;
    };
    if first.kind != TokenKind::Ident {
        return idx;
    }

    let mut i = idx + 1;
    while i + 1 < tokens.len() {
        let op = &tokens[i];
        let next = &tokens[i + 1];
        if !matches!(
            op.kind,
            TokenKind::Minus | TokenKind::Arrow | TokenKind::FatArrow | TokenKind::Tilde
        ) || next.kind != TokenKind::Ident
        {
            break;
        }
        i += 2;
    }
    i
}

fn parse_catch_header_until_period(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    catch_idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> (Vec<NodeId>, usize) {
    let catch_tok = &tokens[catch_idx];
    match scan_until_statement_period(tokens, source, catch_idx + 1) {
        StmtPeriodScan::Found(period_i) => {
            if !has_non_comment_tokens(tokens, catch_idx + 1, period_i) {
                errors.push(crate::ParseError {
                    message: "syntax error: expected exception class after CATCH".to_string(),
                    range: catch_tok.range.start..tokens[period_i].range.end,
                });
            }
            let mut children = vec![token_leaf(b, catch_tok)];
            let mut cursor = catch_idx + 1;
            while cursor < period_i {
                let token = &tokens[cursor];
                if token.kind == TokenKind::Comment {
                    children.push(token_leaf(b, token));
                    cursor += 1;
                    continue;
                }
                if is_keyword(source, token, "into") {
                    children.push(token_leaf(b, token));
                    let target_start = skip_trivia(tokens, cursor + 1);
                    if target_start < period_i {
                        if let Some((inline_decl, next_idx)) =
                            try_parse_catch_inline_data_target(b, source, tokens, target_start)
                        {
                            children.push(inline_decl);
                            for trailing in &tokens[next_idx..period_i] {
                                children.push(token_leaf(b, trailing));
                            }
                        } else {
                            let expr = parse_arithmetic_expr(
                                b,
                                source,
                                &tokens[target_start..period_i],
                                None,
                            );
                            children.push(expr);
                        }
                    }
                    cursor = period_i;
                    continue;
                }
                if is_keyword(source, token, "before") || is_keyword(source, token, "unwind") {
                    children.push(token_leaf(b, token));
                    cursor += 1;
                    continue;
                }
                let type_end = scan_catch_type_ref_end(tokens, cursor);
                if type_end > cursor {
                    children.push(build_type_ref_node(b, source, &tokens[cursor..type_end]));
                    cursor = type_end;
                    continue;
                }
                children.push(token_leaf(b, token));
                cursor += 1;
            }
            children.push(token_leaf(b, &tokens[period_i]));
            (children, period_i + 1)
        }
        StmtPeriodScan::Unterminated { end_exclusive } => {
            let err_end = unterminated_err_end(tokens, end_exclusive, catch_tok.range.end);
            errors.push(crate::ParseError {
                message: "syntax error: expected '.' after CATCH clause".to_string(),
                range: catch_tok.range.start..err_end,
            });
            let err_children = error_token_children(b, tokens, catch_idx, end_exclusive);
            let header = b.branch(
                SyntaxKind::Error,
                catch_tok.range.start..err_end,
                &err_children,
            );
            (
                vec![header],
                next_after_unterminated_scan(tokens, end_exclusive),
            )
        }
    }
}

fn scan_until_top_level_period(tokens: &[Token], start: usize) -> Option<usize> {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut i = start;
    while i < tokens.len() {
        let token = &tokens[i];
        match token.kind {
            TokenKind::Eof => return None,
            TokenKind::Period if paren == 0 && bracket == 0 && brace == 0 => return Some(i),
            TokenKind::LParen => paren += 1,
            TokenKind::RParen if paren > 0 => paren -= 1,
            TokenKind::LBracket => bracket += 1,
            TokenKind::RBracket if bracket > 0 => bracket -= 1,
            TokenKind::LBrace => brace += 1,
            TokenKind::RBrace if brace > 0 => brace -= 1,
            _ => {}
        }
        i += 1;
    }
    None
}

fn catch_system_exceptions_body_start(source: &str, tokens: &[Token], idx: usize) -> Option<usize> {
    let catch_tok = tokens.get(idx)?;
    if !is_keyword(source, catch_tok, "catch") {
        return None;
    }

    let system_idx = skip_trivia(tokens, idx + 1);
    let system_tok = tokens.get(system_idx)?;
    if !is_keyword(source, system_tok, "system") {
        return None;
    }
    if tokens.get(system_idx + 1).map(|token| token.kind) != Some(TokenKind::Minus) {
        return None;
    }
    let exceptions_idx = system_idx + 2;
    let exceptions_tok = tokens.get(exceptions_idx)?;
    if !is_keyword(source, exceptions_tok, "exceptions") {
        return None;
    }
    Some(exceptions_idx + 1)
}

fn parse_catch_system_exceptions_header_until_period(
    b: &mut SyntaxTreeBuilder,
    tokens: &[Token],
    catch_idx: usize,
    body_start_idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> (Vec<NodeId>, usize) {
    let catch_tok = &tokens[catch_idx];
    match scan_until_top_level_period(tokens, body_start_idx) {
        Some(period_i) => {
            let children = error_token_children(b, tokens, catch_idx, period_i + 1);
            (children, period_i + 1)
        }
        None => {
            let end_exclusive = tokens
                .iter()
                .position(|token| token.kind == TokenKind::Eof)
                .unwrap_or(tokens.len());
            let err_end = unterminated_err_end(tokens, end_exclusive, catch_tok.range.end);
            errors.push(crate::ParseError {
                message: "syntax error: expected '.' after CATCH SYSTEM-EXCEPTIONS header"
                    .to_string(),
                range: catch_tok.range.start..err_end,
            });
            let err_children = error_token_children(b, tokens, catch_idx, end_exclusive);
            let header = b.branch(
                SyntaxKind::Error,
                catch_tok.range.start..err_end,
                &err_children,
            );
            (
                vec![header],
                next_after_unterminated_scan(tokens, end_exclusive),
            )
        }
    }
}

fn try_parse_loop_inline_data_target(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
) -> Option<(NodeId, usize)> {
    let data_tok = tokens.get(idx)?;
    if !is_keyword(source, data_tok, "data") {
        return None;
    }
    let lparen = tokens.get(idx + 1)?;
    if lparen.kind != TokenKind::LParen {
        return None;
    }
    let (name, next_idx) = parse_inline_name(b, tokens, idx + 2)?;
    let rparen = tokens.get(next_idx)?;
    if rparen.kind != TokenKind::RParen {
        return None;
    }
    if !inline_name_spacing_is_valid(tokens, idx + 1, idx + 2, next_idx) {
        let mut children = Vec::with_capacity(next_idx - idx + 1);
        for token in &tokens[idx..=next_idx] {
            children.push(token_leaf(b, token));
        }
        return Some((
            b.branch(
                SyntaxKind::Error,
                data_tok.range.start..rparen.range.end,
                &children,
            ),
            next_idx + 1,
        ));
    }
    let data_leaf = token_leaf(b, data_tok);
    let lparen_leaf = token_leaf(b, lparen);
    let rparen_leaf = token_leaf(b, rparen);
    Some((
        b.branch(
            SyntaxKind::DataInlineDecl,
            data_tok.range.start..rparen.range.end,
            &[data_leaf, lparen_leaf, name, rparen_leaf],
        ),
        next_idx + 1,
    ))
}

fn try_parse_catch_inline_data_target(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
) -> Option<(NodeId, usize)> {
    try_parse_loop_inline_data_target(b, source, tokens, idx)
}

fn try_parse_loop_inline_field_symbol_target(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
) -> Option<(NodeId, usize)> {
    let field_tok = tokens.get(idx)?;
    if !is_keyword(source, field_tok, "field")
        || tokens.get(idx + 1).map(|tok| tok.kind) != Some(TokenKind::Minus)
        || !tokens
            .get(idx + 2)
            .is_some_and(|tok| is_keyword(source, tok, "symbol"))
    {
        return None;
    }

    let lparen = tokens.get(idx + 3)?;
    if lparen.kind != TokenKind::LParen {
        return None;
    }
    let (name, next_idx) = parse_inline_name(b, tokens, idx + 4)?;
    let rparen = tokens.get(next_idx)?;
    if rparen.kind != TokenKind::RParen {
        return None;
    }
    if !inline_name_spacing_is_valid(tokens, idx + 3, idx + 4, next_idx) {
        let mut children = Vec::with_capacity(next_idx - idx + 1);
        for token in &tokens[idx..=next_idx] {
            children.push(token_leaf(b, token));
        }
        return Some((
            b.branch(
                SyntaxKind::Error,
                field_tok.range.start..rparen.range.end,
                &children,
            ),
            next_idx + 1,
        ));
    }
    let field_leaf = token_leaf(b, field_tok);
    let minus_leaf = token_leaf(b, &tokens[idx + 1]);
    let symbol_leaf = token_leaf(b, &tokens[idx + 2]);
    let lparen_leaf = token_leaf(b, lparen);
    let rparen_leaf = token_leaf(b, rparen);

    Some((
        b.branch(
            SyntaxKind::FieldSymbolInlineDecl,
            field_tok.range.start..rparen.range.end,
            &[
                field_leaf,
                minus_leaf,
                symbol_leaf,
                lparen_leaf,
                name,
                rparen_leaf,
            ],
        ),
        next_idx + 1,
    ))
}

fn is_loop_group_by_start(source: &str, tokens: &[Token], idx: usize) -> bool {
    tokens
        .get(idx)
        .is_some_and(|token| is_keyword(source, token, "group"))
        && tokens
            .get(skip_trivia(tokens, idx + 1))
            .is_some_and(|token| is_keyword(source, token, "by"))
}

fn is_loop_at_group_start(source: &str, tokens: &[Token], idx: usize) -> bool {
    tokens
        .get(idx)
        .is_some_and(|token| is_keyword(source, token, "group"))
        && !tokens
            .get(skip_trivia(tokens, idx + 1))
            .is_some_and(|token| is_keyword(source, token, "by"))
}

fn loop_clause_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    token.kind == TokenKind::Ident
        && (is_keyword(source, token, "into")
            || is_keyword(source, token, "assigning")
            || is_keyword(source, token, "where")
            || is_keyword(source, token, "using")
            || is_keyword(source, token, "transporting")
            || is_loop_group_by_start(source, tokens, idx)
            || is_keyword(source, token, "from")
            || is_keyword(source, token, "to")
            || is_keyword(source, token, "step")
            || (is_keyword(source, token, "reference")
                && tokens
                    .get(idx + 1)
                    .is_some_and(|next| is_keyword(source, next, "into"))))
}

fn scan_loop_expr_end(source: &str, tokens: &[Token], start: usize, end_exclusive: usize) -> usize {
    let mut idx = start;
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;

    while idx < end_exclusive {
        let token = &tokens[idx];
        if token.kind == TokenKind::Comment {
            idx += 1;
            continue;
        }
        if paren == 0
            && bracket == 0
            && brace == 0
            && (token.kind == TokenKind::Period || loop_clause_starts(source, tokens, idx))
        {
            break;
        }
        match token.kind {
            TokenKind::LParen => paren += 1,
            TokenKind::RParen => paren -= 1,
            TokenKind::LBracket => bracket += 1,
            TokenKind::RBracket => bracket -= 1,
            TokenKind::LBrace => brace += 1,
            TokenKind::RBrace => brace -= 1,
            _ => {}
        }
        idx += 1;
    }

    idx
}

fn loop_group_by_tail_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    is_keyword(source, token, "ascending")
        || is_keyword(source, token, "descending")
        || is_keyword(source, token, "without")
        || loop_clause_starts(source, tokens, idx)
}

fn scan_loop_group_by_expr_end(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> usize {
    let mut idx = start;
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;

    while idx < end_exclusive {
        let token = &tokens[idx];
        if token.kind == TokenKind::Comment {
            idx += 1;
            continue;
        }
        if paren == 0
            && bracket == 0
            && brace == 0
            && (token.kind == TokenKind::Period || loop_group_by_tail_starts(source, tokens, idx))
        {
            break;
        }
        match token.kind {
            TokenKind::LParen => paren += 1,
            TokenKind::RParen => paren -= 1,
            TokenKind::LBracket => bracket += 1,
            TokenKind::RBracket => bracket -= 1,
            TokenKind::LBrace => brace += 1,
            TokenKind::RBrace => brace -= 1,
            _ => {}
        }
        idx += 1;
    }

    idx
}

fn parse_loop_clause_expr(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    expr_start: usize,
    expr_end: usize,
    prev_before_first: Option<&Token>,
    kind: SyntaxKind,
    keyword_tokens: &[usize],
    logical: bool,
) -> Option<NodeId> {
    if expr_start >= expr_end {
        return None;
    }
    let expr = if logical {
        parse_logical_expr(b, source, &tokens[expr_start..expr_end], prev_before_first)
    } else {
        parse_arithmetic_expr(b, source, &tokens[expr_start..expr_end], prev_before_first)
    };
    let start = tokens[*keyword_tokens.first()?].range.start;
    let end = b.span(expr).end;
    let mut children: Vec<NodeId> = keyword_tokens
        .iter()
        .map(|&idx| token_leaf(b, &tokens[idx]))
        .collect();
    children.push(expr);
    Some(b.branch(kind, start..end, &children))
}

fn parse_loop_group_by_clause(
    b: &mut SyntaxTreeBuilder,
    tokens: &[Token],
    group_idx: usize,
    by_idx: usize,
    expr_start: usize,
    expr_end: usize,
) -> Option<NodeId> {
    if expr_start >= expr_end {
        return None;
    }
    let mut children = vec![
        token_leaf(b, &tokens[group_idx]),
        token_leaf(b, &tokens[by_idx]),
    ];
    children.extend(
        tokens[expr_start..expr_end]
            .iter()
            .map(|token| token_leaf(b, token)),
    );
    Some(b.branch(
        SyntaxKind::LoopGroupByClause,
        tokens[group_idx].range.start..tokens[expr_end - 1].range.end,
        &children,
    ))
}

pub fn try_parse_while_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let while_tok = tokens.get(idx)?;
    if !is_keyword(source, while_tok, "while") {
        return None;
    }

    let (mut children, mut next) = match scan_until_statement_period(tokens, source, idx + 1) {
        StmtPeriodScan::Found(period_i) => {
            let cond = if let Some(delim_error) = delimiter_error(tokens, idx + 1, period_i) {
                errors.push(delim_error);
                let err_children = error_token_children(b, tokens, idx + 1, period_i);
                b.branch(
                    SyntaxKind::Error,
                    while_tok.range.end..tokens[period_i].range.start,
                    &err_children,
                )
            } else if has_non_comment_tokens(tokens, idx + 1, period_i) {
                parse_logical_expr(b, source, &tokens[idx + 1..period_i], Some(while_tok))
            } else {
                errors.push(crate::ParseError {
                    message: "syntax error: expected condition after WHILE".to_string(),
                    range: while_tok.range.start..tokens[period_i].range.end,
                });
                let err_children = error_token_children(b, tokens, idx + 1, period_i);
                b.branch(
                    SyntaxKind::Error,
                    while_tok.range.end..tokens[period_i].range.start,
                    &err_children,
                )
            };
            (
                vec![
                    token_leaf(b, while_tok),
                    cond,
                    token_leaf(b, &tokens[period_i]),
                ],
                period_i + 1,
            )
        }
        StmtPeriodScan::Unterminated { end_exclusive } => {
            let err_end = unterminated_err_end(tokens, end_exclusive, while_tok.range.end);
            errors.push(crate::ParseError {
                message: "syntax error: expected '.' after WHILE condition".to_string(),
                range: while_tok.range.start..err_end,
            });
            let err_children = error_token_children(b, tokens, idx, end_exclusive);
            let header = b.branch(
                SyntaxKind::Error,
                while_tok.range.start..err_end,
                &err_children,
            );
            (
                vec![header],
                next_after_unterminated_scan(tokens, end_exclusive),
            )
        }
    };

    let (body, after_body) =
        parse_body_until_keywords(b, source, tokens, next, errors, &["ENDWHILE"]);
    children.extend(body);
    next = after_body;

    let (end_children, next_after, end_pos) = parse_end_keyword(
        b,
        source,
        tokens,
        next,
        while_tok,
        "ENDWHILE",
        "syntax error: expected ENDWHILE",
        errors,
    );
    children.extend(end_children);
    let end = end_pos.max(
        children
            .last()
            .copied()
            .map(|id| b.span(id).end)
            .unwrap_or(while_tok.range.end),
    );
    let node = b.branch(SyntaxKind::WhileStmt, while_tok.range.start..end, &children);
    Some((node, next_after))
}

/// `TIMES` keyword that ends `DO <arith> TIMES .`, not an identifier inside the expression.
fn find_do_times_delimiter(
    source: &str,
    tokens: &[Token],
    mut start: usize,
    period_i: usize,
) -> Option<usize> {
    while start < period_i {
        match tokens[start].kind {
            TokenKind::Comment => start += 1,
            TokenKind::Ident if tokens[start].lexeme(source).eq_ignore_ascii_case("times") => {
                let mut j = start + 1;
                while j < period_i && tokens[j].kind == TokenKind::Comment {
                    j += 1;
                }
                if j == period_i {
                    return Some(start);
                }
                start += 1;
            }
            _ => start += 1,
        }
    }
    None
}

fn trim_trailing_comments(tokens: &[Token], start: usize, end_exclusive: usize) -> usize {
    let mut e = end_exclusive;
    while e > start && tokens[e - 1].kind == TokenKind::Comment {
        e -= 1;
    }
    e
}

fn is_loop_clause_keyword(source: &str, token: &Token) -> bool {
    is_keyword(source, token, "into")
        || is_keyword(source, token, "assigning")
        || is_keyword(source, token, "reference")
        || is_keyword(source, token, "where")
        || is_keyword(source, token, "from")
        || is_keyword(source, token, "to")
        || is_keyword(source, token, "step")
}

/// `DO .` or `DO <arith> TIMES .` — parses the repetition count as an expression (like `WHILE` does
/// for its condition) so identifiers participate in semantic analysis.
fn parse_do_header_until_period(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    do_idx: usize,
    errors: &mut Vec<crate::ParseError>,
    missing_period_message: &str,
) -> (Vec<NodeId>, usize) {
    match scan_until_statement_period(tokens, source, do_idx + 1) {
        StmtPeriodScan::Found(period_i) => {
            let after_do = skip_trivia(tokens, do_idx + 1);
            if let Some(times_i) = find_do_times_delimiter(source, tokens, after_do, period_i) {
                let expr_end = trim_trailing_comments(tokens, after_do, times_i);
                let mut children = vec![token_leaf(b, &tokens[do_idx])];
                if after_do < expr_end {
                    let expr = parse_arithmetic_expr(
                        b,
                        source,
                        &tokens[after_do..expr_end],
                        Some(&tokens[do_idx]),
                    );
                    children.push(expr);
                } else {
                    errors.push(crate::ParseError {
                        message: "syntax error: expected repetition count before TIMES".to_string(),
                        range: tokens[do_idx].range.start..tokens[times_i].range.end,
                    });
                    let err = error_token_children(b, tokens, times_i, times_i + 1);
                    children.push(b.branch(SyntaxKind::Error, tokens[times_i].range.clone(), &err));
                }
                children.push(token_leaf(b, &tokens[times_i]));
                let mut j = times_i + 1;
                while j < period_i {
                    if tokens[j].kind == TokenKind::Comment {
                        children.push(token_leaf(b, &tokens[j]));
                    }
                    j += 1;
                }
                children.push(token_leaf(b, &tokens[period_i]));
                (children, period_i + 1)
            } else {
                let mut children = Vec::with_capacity(period_i.saturating_sub(do_idx) + 1);
                for t in &tokens[do_idx..=period_i] {
                    children.push(token_leaf(b, t));
                }
                (children, period_i + 1)
            }
        }
        StmtPeriodScan::Unterminated { end_exclusive } => {
            let start_tok = &tokens[do_idx];
            let err_end = unterminated_err_end(tokens, end_exclusive, start_tok.range.end);
            errors.push(crate::ParseError {
                message: missing_period_message.to_string(),
                range: start_tok.range.start..err_end,
            });
            let err_children = error_token_children(b, tokens, do_idx, end_exclusive);
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

pub fn try_parse_do_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let do_tok = tokens.get(idx)?;
    if !is_keyword(source, do_tok, "do") {
        return None;
    }

    let (mut children, mut next) = parse_do_header_until_period(
        b,
        source,
        tokens,
        idx,
        errors,
        "syntax error: expected '.' after DO header",
    );
    let (body, after_body) = parse_body_until_keywords(b, source, tokens, next, errors, &["ENDDO"]);
    children.extend(body);
    next = after_body;
    let (end_children, next_after, end_pos) = parse_end_keyword(
        b,
        source,
        tokens,
        next,
        do_tok,
        "ENDDO",
        "syntax error: expected ENDDO",
        errors,
    );
    children.extend(end_children);
    let node = b.branch(SyntaxKind::DoStmt, do_tok.range.start..end_pos, &children);
    Some((node, next_after))
}

fn at_stmt_body_start(source: &str, tokens: &[Token], idx: usize) -> Option<usize> {
    let at_tok = tokens.get(idx)?;
    if !is_keyword(source, at_tok, "at") {
        return None;
    }

    let first = skip_trivia(tokens, idx + 1);
    let first_tok = tokens.get(first)?;
    if is_keyword(source, first_tok, "first") || is_keyword(source, first_tok, "last") {
        return Some(first + 1);
    }
    if is_keyword(source, first_tok, "new") {
        return Some(skip_trivia(tokens, first + 1));
    }
    if is_keyword(source, first_tok, "end") {
        let of_idx = skip_trivia(tokens, first + 1);
        let of_tok = tokens.get(of_idx)?;
        if is_keyword(source, of_tok, "of") {
            return Some(skip_trivia(tokens, of_idx + 1));
        }
    }
    None
}

pub fn try_parse_at_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let at_tok = tokens.get(idx)?;
    let body_start_idx = at_stmt_body_start(source, tokens, idx)?;

    let (mut children, mut next) = parse_header_until_period(
        b,
        source,
        tokens,
        idx,
        body_start_idx,
        errors,
        "syntax error: expected '.' after AT header",
    );
    let (body, after_body) = parse_body_until_keywords(b, source, tokens, next, errors, &["ENDAT"]);
    children.extend(body);
    next = after_body;
    let (end_children, next_after, end_pos) = parse_end_keyword(
        b,
        source,
        tokens,
        next,
        at_tok,
        "ENDAT",
        "syntax error: expected ENDAT",
        errors,
    );
    children.extend(end_children);
    let node = b.branch(SyntaxKind::AtStmt, at_tok.range.start..end_pos, &children);
    Some((node, next_after))
}

pub fn try_parse_loop_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let loop_tok = tokens.get(idx)?;
    if !is_keyword(source, loop_tok, "loop") {
        return None;
    }

    let at_tok = tokens.get(idx + 1)?;
    if !is_keyword(source, at_tok, "at") {
        return None;
    }

    let (mut children, mut next) = match scan_until_statement_period(tokens, source, idx + 2) {
        StmtPeriodScan::Found(period_i) => {
            let mut children = vec![token_leaf(b, loop_tok), token_leaf(b, at_tok)];
            let mut cursor = idx + 2;
            let first_operand = skip_trivia(tokens, cursor);
            if first_operand >= period_i || is_loop_clause_keyword(source, &tokens[first_operand]) {
                errors.push(crate::ParseError {
                    message: "syntax error: expected loop source after LOOP AT".to_string(),
                    range: at_tok.range.start..tokens[period_i].range.end,
                });
            }
            if is_loop_at_group_start(source, tokens, first_operand) {
                let group_tok = &tokens[first_operand];
                let group_start = skip_trivia(tokens, first_operand + 1);
                let group_end = scan_loop_expr_end(source, tokens, group_start, period_i);
                if let Some(group_clause) = parse_loop_clause_expr(
                    b,
                    source,
                    tokens,
                    group_start,
                    group_end,
                    Some(group_tok),
                    SyntaxKind::LoopAtGroupClause,
                    &[first_operand],
                    false,
                ) {
                    children.push(group_clause);
                }
                cursor = group_end;
            } else {
                let source_end = scan_loop_expr_end(source, tokens, cursor, period_i);
                if let Some(source_clause) = parse_loop_clause_expr(
                    b,
                    source,
                    tokens,
                    cursor,
                    source_end,
                    Some(at_tok),
                    SyntaxKind::LoopSourceClause,
                    &[idx + 1],
                    false,
                ) {
                    children.push(source_clause);
                }
                cursor = source_end;
            }

            while cursor < period_i {
                let token = &tokens[cursor];
                if is_loop_group_by_start(source, tokens, cursor) {
                    let by_idx = skip_trivia(tokens, cursor + 1);
                    let expr_start = skip_trivia(tokens, by_idx + 1);
                    let expr_end =
                        scan_loop_group_by_expr_end(source, tokens, expr_start, period_i);
                    if let Some(clause) =
                        parse_loop_group_by_clause(b, tokens, cursor, by_idx, expr_start, expr_end)
                    {
                        children.push(clause);
                    }
                    cursor = expr_end;
                    continue;
                }
                if is_keyword(source, token, "into") {
                    let target_start = skip_trivia(tokens, cursor + 1);
                    let target_end = scan_loop_expr_end(source, tokens, target_start, period_i);
                    let mut clause_children = vec![token_leaf(b, token)];
                    if let Some((inline_data, next_idx)) =
                        try_parse_loop_inline_data_target(b, source, tokens, target_start)
                    {
                        clause_children.push(inline_data);
                        cursor = next_idx;
                    } else if target_start < target_end {
                        let expr = parse_arithmetic_expr(
                            b,
                            source,
                            &tokens[target_start..target_end],
                            Some(token),
                        );
                        clause_children.push(expr);
                        cursor = target_end;
                    } else {
                        errors.push(crate::ParseError {
                            message: "syntax error: expected target after INTO".to_string(),
                            range: token.range.start..tokens[period_i].range.start,
                        });
                        cursor = target_start;
                    }
                    let end = clause_children
                        .last()
                        .copied()
                        .map(|id| b.span(id).end)
                        .unwrap_or(token.range.end);
                    children.push(b.branch(
                        SyntaxKind::LoopIntoClause,
                        token.range.start..end,
                        &clause_children,
                    ));
                    continue;
                }
                if is_keyword(source, token, "assigning") {
                    let target_start = skip_trivia(tokens, cursor + 1);
                    let target_end = scan_loop_expr_end(source, tokens, target_start, period_i);
                    let mut clause_children = vec![token_leaf(b, token)];
                    if let Some((inline_decl, next_idx)) =
                        try_parse_loop_inline_field_symbol_target(b, source, tokens, target_start)
                    {
                        clause_children.push(inline_decl);
                        cursor = next_idx;
                    } else if target_start < target_end {
                        let expr = parse_arithmetic_expr(
                            b,
                            source,
                            &tokens[target_start..target_end],
                            Some(token),
                        );
                        clause_children.push(expr);
                        cursor = target_end;
                    } else {
                        errors.push(crate::ParseError {
                            message: "syntax error: expected target after ASSIGNING".to_string(),
                            range: token.range.start..tokens[period_i].range.start,
                        });
                        cursor = target_start;
                    }
                    let end = clause_children
                        .last()
                        .copied()
                        .map(|id| b.span(id).end)
                        .unwrap_or(token.range.end);
                    children.push(b.branch(
                        SyntaxKind::LoopAssigningClause,
                        token.range.start..end,
                        &clause_children,
                    ));
                    continue;
                }
                if is_keyword(source, token, "reference")
                    && tokens
                        .get(cursor + 1)
                        .is_some_and(|next| is_keyword(source, next, "into"))
                {
                    let into_tok = &tokens[cursor + 1];
                    let target_start = skip_trivia(tokens, cursor + 2);
                    let target_end = scan_loop_expr_end(source, tokens, target_start, period_i);
                    let mut clause_children = vec![token_leaf(b, token), token_leaf(b, into_tok)];
                    if target_start < target_end {
                        let expr = parse_arithmetic_expr(
                            b,
                            source,
                            &tokens[target_start..target_end],
                            Some(into_tok),
                        );
                        clause_children.push(expr);
                    } else {
                        errors.push(crate::ParseError {
                            message: "syntax error: expected target after REFERENCE INTO"
                                .to_string(),
                            range: token.range.start..tokens[period_i].range.start,
                        });
                    }
                    let end = clause_children
                        .last()
                        .copied()
                        .map(|id| b.span(id).end)
                        .unwrap_or(into_tok.range.end);
                    children.push(b.branch(
                        SyntaxKind::LoopReferenceIntoClause,
                        token.range.start..end,
                        &clause_children,
                    ));
                    cursor = target_end;
                    continue;
                }
                let clause_kind = if is_keyword(source, token, "where") {
                    Some((SyntaxKind::LoopWhereClause, true))
                } else if is_keyword(source, token, "from") {
                    Some((SyntaxKind::LoopFromClause, false))
                } else if is_keyword(source, token, "to") {
                    Some((SyntaxKind::LoopToClause, false))
                } else if is_keyword(source, token, "step") {
                    Some((SyntaxKind::LoopStepClause, false))
                } else {
                    None
                };
                if let Some((clause_kind, logical)) = clause_kind {
                    let expr_start = skip_trivia(tokens, cursor + 1);
                    let expr_end = scan_loop_expr_end(source, tokens, expr_start, period_i);
                    if let Some(clause) = parse_loop_clause_expr(
                        b,
                        source,
                        tokens,
                        expr_start,
                        expr_end,
                        Some(token),
                        clause_kind,
                        &[cursor],
                        logical,
                    ) {
                        children.push(clause);
                    } else {
                        errors.push(crate::ParseError {
                            message: format!(
                                "syntax error: expected expression after {}",
                                token.lexeme(source).to_ascii_uppercase()
                            ),
                            range: token.range.start..tokens[period_i].range.start,
                        });
                    }
                    cursor = expr_end;
                    continue;
                }
                children.push(token_leaf(b, token));
                cursor += 1;
            }
            children.push(token_leaf(b, &tokens[period_i]));
            (children, period_i + 1)
        }
        StmtPeriodScan::Unterminated { end_exclusive } => {
            let err_end = unterminated_err_end(tokens, end_exclusive, loop_tok.range.end);
            errors.push(crate::ParseError {
                message: "syntax error: expected '.' after LOOP header".to_string(),
                range: loop_tok.range.start..err_end,
            });
            let err_children = error_token_children(b, tokens, idx, end_exclusive);
            let header = b.branch(
                SyntaxKind::Error,
                loop_tok.range.start..err_end,
                &err_children,
            );
            (
                vec![header],
                next_after_unterminated_scan(tokens, end_exclusive),
            )
        }
    };
    let (body, after_body) =
        parse_body_until_keywords(b, source, tokens, next, errors, &["ENDLOOP"]);
    children.extend(body);
    next = after_body;
    let (end_children, next_after, end_pos) = parse_end_keyword(
        b,
        source,
        tokens,
        next,
        loop_tok,
        "ENDLOOP",
        "syntax error: expected ENDLOOP",
        errors,
    );
    children.extend(end_children);
    let node = b.branch(
        SyntaxKind::LoopStmt,
        loop_tok.range.start..end_pos,
        &children,
    );
    Some((node, next_after))
}

pub fn try_parse_case_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let case_tok = tokens.get(idx)?;
    if !is_keyword(source, case_tok, "case") {
        return None;
    }

    let (mut children, mut next) = match scan_until_statement_period(tokens, source, idx + 1) {
        StmtPeriodScan::Found(period_i) => {
            let expr = if let Some(delim_error) = delimiter_error(tokens, idx + 1, period_i) {
                errors.push(delim_error);
                let err_children = error_token_children(b, tokens, idx + 1, period_i);
                b.branch(
                    SyntaxKind::Error,
                    case_tok.range.end..tokens[period_i].range.start,
                    &err_children,
                )
            } else if has_non_comment_tokens(tokens, idx + 1, period_i) {
                parse_arithmetic_expr(b, source, &tokens[idx + 1..period_i], Some(case_tok))
            } else {
                errors.push(crate::ParseError {
                    message: "syntax error: expected expression after CASE".to_string(),
                    range: case_tok.range.start..tokens[period_i].range.end,
                });
                let err_children = error_token_children(b, tokens, idx + 1, period_i);
                b.branch(
                    SyntaxKind::Error,
                    case_tok.range.end..tokens[period_i].range.start,
                    &err_children,
                )
            };
            (
                vec![
                    token_leaf(b, case_tok),
                    expr,
                    token_leaf(b, &tokens[period_i]),
                ],
                period_i + 1,
            )
        }
        StmtPeriodScan::Unterminated { end_exclusive } => {
            let err_end = unterminated_err_end(tokens, end_exclusive, case_tok.range.end);
            errors.push(crate::ParseError {
                message: "syntax error: expected '.' after CASE expression".to_string(),
                range: case_tok.range.start..err_end,
            });
            let err_children = error_token_children(b, tokens, idx, end_exclusive);
            let header = b.branch(
                SyntaxKind::Error,
                case_tok.range.start..err_end,
                &err_children,
            );
            (
                vec![header],
                next_after_unterminated_scan(tokens, end_exclusive),
            )
        }
    };

    while let Some(Boundary::Keyword("WHEN")) =
        scan_boundary_keywords(source, tokens, next, &["WHEN", "ENDCASE"])
    {
        let when_idx = skip_trivia(tokens, next);
        let when_tok = &tokens[when_idx];
        if let StmtPeriodScan::Found(period_i) =
            scan_until_statement_period(tokens, source, when_idx + 1)
        {
            if let Some(delim_error) = delimiter_error(tokens, when_idx + 1, period_i) {
                errors.push(delim_error);
            }
            if !has_non_comment_tokens(tokens, when_idx + 1, period_i) {
                errors.push(crate::ParseError {
                    message: "syntax error: expected expression after WHEN".to_string(),
                    range: when_tok.range.start..tokens[period_i].range.end,
                });
            }
        }
        let (mut when_children, body_start) = parse_header_until_period(
            b,
            source,
            tokens,
            when_idx,
            when_idx + 1,
            errors,
            "syntax error: expected '.' after WHEN branch",
        );
        let (body, after_body) =
            parse_body_until_keywords(b, source, tokens, body_start, errors, &["WHEN", "ENDCASE"]);
        when_children.extend(body);
        let end = when_children
            .last()
            .copied()
            .map(|id| b.span(id).end)
            .unwrap_or(when_tok.range.end);
        let clause = b.branch(
            SyntaxKind::WhenClause,
            when_tok.range.start..end,
            &when_children,
        );
        children.push(clause);
        next = after_body;
    }

    let (end_children, next_after, end_pos) = parse_end_keyword(
        b,
        source,
        tokens,
        next,
        case_tok,
        "ENDCASE",
        "syntax error: expected ENDCASE",
        errors,
    );
    children.extend(end_children);
    let node = b.branch(
        SyntaxKind::CaseStmt,
        case_tok.range.start..end_pos,
        &children,
    );
    Some((node, next_after))
}

pub fn try_parse_try_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let try_tok = tokens.get(idx)?;
    if !is_keyword(source, try_tok, "try") {
        return None;
    }

    let (mut children, mut next) = parse_header_until_period(
        b,
        source,
        tokens,
        idx,
        idx + 1,
        errors,
        "syntax error: expected '.' after TRY",
    );
    let (body, after_body) = parse_body_until_keywords(
        b,
        source,
        tokens,
        next,
        errors,
        &["CATCH", "CLEANUP", "ENDTRY"],
    );
    children.extend(body);
    next = after_body;

    while matches!(
        scan_boundary_keywords(source, tokens, next, &["CATCH"]),
        Some(Boundary::Keyword("CATCH"))
    ) {
        let catch_idx = skip_trivia(tokens, next);
        let catch_tok = &tokens[catch_idx];
        let (mut catch_children, body_start) =
            parse_catch_header_until_period(b, source, tokens, catch_idx, errors);
        let (catch_body, after_catch) = parse_body_until_keywords(
            b,
            source,
            tokens,
            body_start,
            errors,
            &["CATCH", "CLEANUP", "ENDTRY"],
        );
        catch_children.extend(catch_body);
        let end = catch_children
            .last()
            .copied()
            .map(|id| b.span(id).end)
            .unwrap_or(catch_tok.range.end);
        let clause = b.branch(
            SyntaxKind::CatchClause,
            catch_tok.range.start..end,
            &catch_children,
        );
        children.push(clause);
        next = after_catch;
    }

    if matches!(
        scan_boundary_keywords(source, tokens, next, &["CLEANUP"]),
        Some(Boundary::Keyword("CLEANUP"))
    ) {
        let cleanup_idx = skip_trivia(tokens, next);
        let cleanup_tok = &tokens[cleanup_idx];
        let (mut cleanup_children, body_start) = parse_header_until_period(
            b,
            source,
            tokens,
            cleanup_idx,
            cleanup_idx + 1,
            errors,
            "syntax error: expected '.' after CLEANUP",
        );
        let (cleanup_body, after_cleanup) =
            parse_body_until_keywords(b, source, tokens, body_start, errors, &["ENDTRY"]);
        cleanup_children.extend(cleanup_body);
        let end = cleanup_children
            .last()
            .copied()
            .map(|id| b.span(id).end)
            .unwrap_or(cleanup_tok.range.end);
        let clause = b.branch(
            SyntaxKind::CleanupClause,
            cleanup_tok.range.start..end,
            &cleanup_children,
        );
        children.push(clause);
        next = after_cleanup;
    }

    let (end_children, next_after, end_pos) = parse_end_keyword(
        b,
        source,
        tokens,
        next,
        try_tok,
        "ENDTRY",
        "syntax error: expected ENDTRY",
        errors,
    );
    children.extend(end_children);
    let node = b.branch(SyntaxKind::TryStmt, try_tok.range.start..end_pos, &children);
    Some((node, next_after))
}

pub fn try_parse_catch_system_exceptions_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let catch_tok = tokens.get(idx)?;
    let body_start_idx = catch_system_exceptions_body_start(source, tokens, idx)?;
    let (mut children, mut next) =
        parse_catch_system_exceptions_header_until_period(b, tokens, idx, body_start_idx, errors);
    let (body, after_body) =
        parse_body_until_keywords(b, source, tokens, next, errors, &["ENDCATCH"]);
    children.extend(body);
    next = after_body;

    let (end_children, next_after, end_pos) = parse_end_keyword(
        b,
        source,
        tokens,
        next,
        catch_tok,
        "ENDCATCH",
        "syntax error: expected ENDCATCH",
        errors,
    );
    children.extend(end_children);
    let node_end = children
        .last()
        .copied()
        .map(|id| b.span(id).end)
        .unwrap_or(end_pos)
        .max(end_pos);
    let node = b.branch(
        SyntaxKind::CatchSystemExceptionsStmt,
        catch_tok.range.start..node_end,
        &children,
    );
    Some((node, next_after))
}

#[cfg(test)]
mod tests {
    use abap_ast::SyntaxKind;

    #[test]
    fn parses_while_loop() {
        let parsed = crate::parse("WHILE lv > 0. lv = lv - 1. ENDWHILE.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::WhileStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::AssignStmt),
            1
        );
    }

    #[test]
    fn parses_case_when() {
        let parsed = crate::parse("CASE lv. WHEN 1. lv = 1. WHEN OTHERS. lv = 2. ENDCASE.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::CaseStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::WhenClause),
            2
        );
    }

    #[test]
    fn parses_try_catch_cleanup() {
        let parsed = crate::parse("TRY. a = 1. CATCH cx_root. b = 2. CLEANUP. c = 3. ENDTRY.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::TryStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::CatchClause),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::CleanupClause),
            1
        );
    }

    #[test]
    fn parses_catch_type_and_into_target_with_pragma() {
        let parsed = crate::parse(
            "TRY. WRITE 'x'. CATCH cx_sxml_parse_error INTO lo_parse_error ##no_handler. WRITE 'y'. ENDTRY.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::TryStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::CatchClause), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::TypeRefSimple), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::ExprIdent), 1);
    }

    #[test]
    fn parses_catch_into_inline_data_target_with_pragma() {
        let parsed = crate::parse(
            "TRY. WRITE 'x'. CATCH cx_root INTO DATA(lo_root) ##catch_all. WRITE lo_root->get_text( ). ENDTRY.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::TryStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::CatchClause), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::TypeRefSimple), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::DataInlineDecl), 1);
    }

    #[test]
    fn parses_resumable_exception_flow() {
        let parsed = crate::parse(
            "TRY. RAISE RESUMABLE EXCEPTION TYPE cx_demo. CATCH BEFORE UNWIND cx_demo. RESUME. CATCH cx_root. RETRY. ENDTRY.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::TryStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::CatchClause), 2);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::RaiseStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::ResumeStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::RetryStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::TypeRefSimple), 3);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_catch_system_exceptions_block() {
        let parsed =
            crate::parse("CATCH SYSTEM-EXCEPTIONS move_cast_error = 1.\n  lv = 1.\nENDCATCH.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::CatchSystemExceptionsStmt),
            1
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::AssignStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_catch_system_exceptions_multiline_mappings() {
        let parsed = crate::parse(
            "CATCH SYSTEM-EXCEPTIONS\n  dataset_too_many_files = 6\n  open_dataset_no_authority = 7\n  open_pipe_no_authority = 8\n  dataset_no_pipe = 9.\n  IF lv = 1.\n    lv = 2.\n  ENDIF.\nENDCATCH.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::CatchSystemExceptionsStmt),
            1
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::IfStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::AssignStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_loop_header_with_multiline_where_condition() {
        let parsed = crate::parse(
            "LOOP AT it_reg_attr TRANSPORTING NO FIELDS\n  WHERE reg_valid_from IS INITIAL OR\n        reg_valid_from = ''.\nENDLOOP.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::LoopStmt),
            1
        );
    }

    #[test]
    fn parses_loop_group_by_and_loop_at_group_clauses() {
        let parsed = crate::parse(
            "LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<row>) WHERE status = space GROUP BY <row>-archivekey.\n  LOOP AT GROUP <row> ASSIGNING FIELD-SYMBOL(<member>).\n  ENDLOOP.\nENDLOOP.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::LoopStmt), 2);
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::LoopGroupByClause),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::LoopAtGroupClause),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::LoopAssigningClause),
            2
        );
    }

    #[test]
    fn parses_do_unconditional_and_times() {
        let parsed = crate::parse("DO. EXIT. ENDDO.\nDO 7 TIMES. CONTINUE. ENDDO.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::DoStmt), 2);
    }

    #[test]
    fn parses_at_group_processing_blocks_inside_loop() {
        let parsed = crate::parse(
            "LOOP AT itab INTO wa.\n  AT NEW a.\n    x = 1.\n  ENDAT.\n  AT LAST.\n    y = 2.\n  ENDAT.\nENDLOOP.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::LoopStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::AtStmt), 2);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::EndAtStmt), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
    }

    #[test]
    fn parses_do_times_count_as_expression_for_semantics() {
        let parsed = crate::parse("DO lv_max_len TIMES. ENDDO.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert!(
            parsed.file.count_kind(root, SyntaxKind::ExprIdent) >= 1,
            "expected repetition count identifier in structured DO header"
        );
    }

    #[test]
    fn parses_nested_do_enddo() {
        let parsed = crate::parse("DO lv_max TIMES.\nDO 7 TIMES.\na = 1.\nENDDO.\nb = 2.\nENDDO.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::DoStmt), 2);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::AssignStmt), 2);
    }
}
