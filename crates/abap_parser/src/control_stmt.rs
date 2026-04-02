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

    let (mut children, mut next) = parse_header_until_period(
        b,
        source,
        tokens,
        idx,
        idx + 1,
        errors,
        "syntax error: expected '.' after LOOP header",
    );
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
