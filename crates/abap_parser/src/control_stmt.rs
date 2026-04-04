use abap_ast::SyntaxKind;
use abap_ast::arena::{NodeId, SyntaxTreeBuilder};
use abap_lexer::{Token, TokenKind};

use crate::block_helpers::{
    Boundary, error_token_children, is_keyword, next_after_unterminated_scan,
    parse_body_until_keywords, parse_header_until_period, recover_skip_after_keyword,
    scan_boundary_keywords, skip_trivia,
};
use crate::expr::{parse_arithmetic_expr, parse_logical_expr};
use crate::stmt_period::{StmtPeriodScan, scan_until_statement_period, unterminated_err_end};
use crate::syntax::token_leaf;

fn parse_end_keyword(
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
            range: start_tok.range.start..start_tok.range.end,
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
            || is_keyword(source, token, "group")
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
            let cond = parse_logical_expr(b, source, &tokens[idx + 1..period_i], Some(while_tok));
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

    let (mut children, mut next) = parse_header_until_period(
        b,
        source,
        tokens,
        idx,
        idx + 1,
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

            while cursor < period_i {
                let token = &tokens[cursor];
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
            let expr = parse_arithmetic_expr(b, source, &tokens[idx + 1..period_i], Some(case_tok));
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

    loop {
        match scan_boundary_keywords(source, tokens, next, &["WHEN", "ENDCASE"]) {
            Some(Boundary::Keyword("WHEN")) => {
                let when_idx = skip_trivia(tokens, next);
                let when_tok = &tokens[when_idx];
                let (mut when_children, body_start) = parse_header_until_period(
                    b,
                    source,
                    tokens,
                    when_idx,
                    when_idx + 1,
                    errors,
                    "syntax error: expected '.' after WHEN branch",
                );
                let (body, after_body) = parse_body_until_keywords(
                    b,
                    source,
                    tokens,
                    body_start,
                    errors,
                    &["WHEN", "ENDCASE"],
                );
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
            _ => break,
        }
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
        let (mut catch_children, body_start) = parse_header_until_period(
            b,
            source,
            tokens,
            catch_idx,
            catch_idx + 1,
            errors,
            "syntax error: expected '.' after CATCH clause",
        );
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
}
