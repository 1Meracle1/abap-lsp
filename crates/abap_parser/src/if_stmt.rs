//! `IF cond. … [ELSEIF cond. …]* [ELSE. …] ENDIF.` (classic ABAP).

use abap_ast::SyntaxKind;
use abap_ast::arena::{NodeId, SyntaxTreeBuilder};
use abap_lexer::{Token, TokenKind};

use crate::block_helpers::ensure_forward_progress;
use crate::expr::parse_logical_expr;
use crate::stmt_period::{
    StmtPeriodScan, delimiter_error, has_non_comment_tokens, scan_until_statement_period,
    unterminated_err_end,
};
use crate::syntax::token_leaf;

#[derive(Clone, Copy)]
enum IfScan {
    Elseif,
    Else,
    Endif,
}

fn scan_if_boundary(source: &str, tokens: &[Token], mut idx: usize) -> Option<IfScan> {
    while idx < tokens.len() {
        match tokens[idx].kind {
            TokenKind::Comment => idx += 1,
            TokenKind::Ident => {
                let s = tokens[idx].lexeme(source);
                if s.eq_ignore_ascii_case("elseif") {
                    return Some(IfScan::Elseif);
                }
                if s.eq_ignore_ascii_case("else") {
                    return Some(IfScan::Else);
                }
                if s.eq_ignore_ascii_case("endif") {
                    return Some(IfScan::Endif);
                }
                return None;
            }
            TokenKind::Eof => return None,
            _ => return None,
        }
    }
    None
}

fn skip_trivia(tokens: &[Token], mut idx: usize) -> usize {
    while idx < tokens.len() && tokens[idx].kind == TokenKind::Comment {
        idx += 1;
    }
    idx
}

fn next_after_unterminated_scan(tokens: &[Token], end_exclusive: usize) -> usize {
    if tokens.get(end_exclusive).map(|t| t.kind) == Some(TokenKind::Eof) {
        tokens.len()
    } else {
        end_exclusive
    }
}

fn error_token_children(
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

fn recover_skip_after_endif(source: &str, tokens: &[Token], mut idx: usize) -> usize {
    while idx < tokens.len() {
        if tokens[idx].kind == TokenKind::Ident
            && tokens[idx].lexeme(source).eq_ignore_ascii_case("endif")
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

fn parse_body_until(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    mut idx: usize,
    errors: &mut Vec<crate::ParseError>,
    stop_at_elseif: bool,
    stop_at_else: bool,
    stop_at_endif: bool,
) -> (Vec<NodeId>, usize) {
    let mut nodes = Vec::new();
    loop {
        if let Some(boundary) = scan_if_boundary(source, tokens, idx) {
            let stop = match boundary {
                IfScan::Elseif => stop_at_elseif,
                IfScan::Else => stop_at_else,
                IfScan::Endif => stop_at_endif,
            };
            if stop {
                break;
            }
        }
        if idx >= tokens.len() || tokens[idx].kind == TokenKind::Eof {
            break;
        }
        let (n, next) = crate::parse_file_level_item(b, source, tokens, idx, errors);
        nodes.push(n);
        idx = ensure_forward_progress(tokens, idx, next);
    }
    (nodes, idx)
}

/// If `tokens[idx]` starts `IF cond .`, builds an [`IfStmt`] through `ENDIF .`.
pub fn try_parse_if_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let if_tok = tokens.get(idx)?;
    if if_tok.kind != TokenKind::Ident || !if_tok.lexeme(source).eq_ignore_ascii_case("if") {
        return None;
    }

    let cond_start = idx + 1;
    let (mut children, mut next) = match scan_until_statement_period(tokens, source, cond_start) {
        StmtPeriodScan::Found(period_i) => {
            let period_tok = tokens.get(period_i)?;
            let cond_tokens = &tokens[cond_start..period_i];
            let prev_before_cond = idx.checked_sub(1).and_then(|j| tokens.get(j));
            let cond = if let Some(delim_error) = delimiter_error(tokens, cond_start, period_i) {
                errors.push(delim_error);
                let err_children = error_token_children(b, tokens, cond_start, period_i);
                b.branch(
                    SyntaxKind::Error,
                    if_tok.range.end..period_tok.range.start,
                    &err_children,
                )
            } else if has_non_comment_tokens(tokens, cond_start, period_i) {
                parse_logical_expr(b, source, cond_tokens, prev_before_cond)
            } else {
                errors.push(crate::ParseError {
                    message: "syntax error: expected condition after IF".to_string(),
                    range: if_tok.range.start..period_tok.range.end,
                });
                let err_children = error_token_children(b, tokens, cond_start, period_i);
                b.branch(
                    SyntaxKind::Error,
                    if_tok.range.end..period_tok.range.start,
                    &err_children,
                )
            };
            (
                vec![token_leaf(b, if_tok), cond, token_leaf(b, period_tok)],
                period_i + 1,
            )
        }
        StmtPeriodScan::Unterminated { end_exclusive } => {
            let err_end = unterminated_err_end(tokens, end_exclusive, if_tok.range.end);
            errors.push(crate::ParseError {
                message: "syntax error: expected '.' after IF condition".to_string(),
                range: if_tok.range.start..err_end,
            });
            let header_children = error_token_children(b, tokens, idx, end_exclusive);
            let header = b.branch(
                SyntaxKind::Error,
                if_tok.range.start..err_end,
                &header_children,
            );
            (
                vec![header],
                next_after_unterminated_scan(tokens, end_exclusive),
            )
        }
    };

    let (body, after_body) = parse_body_until(b, source, tokens, next, errors, true, true, true);
    children.extend(body);
    next = after_body;

    while matches!(scan_if_boundary(source, tokens, next), Some(IfScan::Elseif)) {
        let elseif_idx = skip_trivia(tokens, next);
        let elseif_tok = &tokens[elseif_idx];
        let cond_start_e = elseif_idx + 1;
        let (mut elseif_children, body_start, clause_start) =
            match scan_until_statement_period(tokens, source, cond_start_e) {
                StmtPeriodScan::Found(period_e) => {
                    let period_et = tokens.get(period_e)?;
                    let cond_e_tokens = &tokens[cond_start_e..period_e];
                    let cond_e = if let Some(delim_error) =
                        delimiter_error(tokens, cond_start_e, period_e)
                    {
                        errors.push(delim_error);
                        let err_children = error_token_children(b, tokens, cond_start_e, period_e);
                        b.branch(
                            SyntaxKind::Error,
                            elseif_tok.range.end..period_et.range.start,
                            &err_children,
                        )
                    } else if has_non_comment_tokens(tokens, cond_start_e, period_e) {
                        parse_logical_expr(b, source, cond_e_tokens, Some(elseif_tok))
                    } else {
                        errors.push(crate::ParseError {
                            message: "syntax error: expected condition after ELSEIF".to_string(),
                            range: elseif_tok.range.start..period_et.range.end,
                        });
                        let err_children = error_token_children(b, tokens, cond_start_e, period_e);
                        b.branch(
                            SyntaxKind::Error,
                            elseif_tok.range.end..period_et.range.start,
                            &err_children,
                        )
                    };
                    (
                        vec![token_leaf(b, elseif_tok), cond_e, token_leaf(b, period_et)],
                        period_e + 1,
                        elseif_tok.range.start,
                    )
                }
                StmtPeriodScan::Unterminated { end_exclusive } => {
                    let err_end = unterminated_err_end(tokens, end_exclusive, elseif_tok.range.end);
                    errors.push(crate::ParseError {
                        message: "syntax error: expected '.' after ELSEIF condition".to_string(),
                        range: elseif_tok.range.start..err_end,
                    });
                    let header_children =
                        error_token_children(b, tokens, elseif_idx, end_exclusive);
                    let header = b.branch(
                        SyntaxKind::Error,
                        elseif_tok.range.start..err_end,
                        &header_children,
                    );
                    (
                        vec![header],
                        next_after_unterminated_scan(tokens, end_exclusive),
                        elseif_tok.range.start,
                    )
                }
            };
        next = body_start;
        let (arm_body, after_arm) =
            parse_body_until(b, source, tokens, next, errors, true, true, true);
        next = after_arm;
        let end_span = arm_body
            .last()
            .copied()
            .map(|id| b.span(id).end)
            .unwrap_or_else(|| b.span(*elseif_children.last().expect("ELSEIF header")).end);
        elseif_children.extend(arm_body);
        let clause = b.branch(
            SyntaxKind::ElseifClause,
            clause_start..end_span,
            &elseif_children,
        );
        children.push(clause);
    }

    if matches!(scan_if_boundary(source, tokens, next), Some(IfScan::Else)) {
        let else_idx = skip_trivia(tokens, next);
        let else_tok = &tokens[else_idx];
        let mut j = else_idx + 1;
        j = skip_trivia(tokens, j);
        let Some(period_else) = tokens.get(j) else {
            errors.push(crate::ParseError {
                message: "syntax error: expected '.' after ELSE".to_string(),
                range: else_tok.range.clone(),
            });
            let recover = recover_skip_after_endif(source, tokens, next);
            let node = b.branch(
                SyntaxKind::IfStmt,
                if_tok.range.start..else_tok.range.end,
                &children,
            );
            return Some((node, recover));
        };
        if period_else.kind != TokenKind::Period {
            errors.push(crate::ParseError {
                message: "syntax error: expected '.' after ELSE".to_string(),
                range: else_tok.range.start..period_else.range.end,
            });
            let recover = recover_skip_after_endif(source, tokens, next);
            let node = b.branch(
                SyntaxKind::IfStmt,
                if_tok.range.start..period_else.range.end,
                &children,
            );
            return Some((node, recover));
        }
        let mut else_children = vec![token_leaf(b, else_tok), token_leaf(b, period_else)];
        next = j + 1;
        let (else_body, after_else) =
            parse_body_until(b, source, tokens, next, errors, false, false, true);
        next = after_else;
        let end_span = else_body
            .last()
            .copied()
            .map(|id| b.span(id).end)
            .unwrap_or(period_else.range.end);
        else_children.extend(else_body);
        let clause = b.branch(
            SyntaxKind::ElseClause,
            else_tok.range.start..end_span,
            &else_children,
        );
        children.push(clause);
    }

    let endif_idx = skip_trivia(tokens, next);
    let Some(endif_tok) = tokens.get(endif_idx) else {
        errors.push(crate::ParseError {
            message: "syntax error: expected ENDIF".to_string(),
            range: if_tok.range.start..source.len(),
        });
        let recover = recover_skip_after_endif(source, tokens, idx + 1);
        let node = b.branch(
            SyntaxKind::IfStmt,
            if_tok.range.start..source.len(),
            &children,
        );
        return Some((node, recover));
    };

    if endif_tok.kind != TokenKind::Ident || !endif_tok.lexeme(source).eq_ignore_ascii_case("endif")
    {
        errors.push(crate::ParseError {
            message: "syntax error: expected ENDIF".to_string(),
            range: if_tok.range.start..endif_tok.range.end,
        });
        let recover = recover_skip_after_endif(source, tokens, next);
        let node = b.branch(
            SyntaxKind::IfStmt,
            if_tok.range.start..endif_tok.range.end,
            &children,
        );
        return Some((node, recover));
    }

    let mut k = endif_idx + 1;
    k = skip_trivia(tokens, k);
    let Some(final_period) = tokens.get(k) else {
        errors.push(crate::ParseError {
            message: "syntax error: expected '.' after ENDIF".to_string(),
            range: endif_tok.range.clone(),
        });
        let recover = recover_skip_after_endif(source, tokens, endif_idx);
        let node = b.branch(
            SyntaxKind::IfStmt,
            if_tok.range.start..endif_tok.range.end,
            &children,
        );
        return Some((node, recover));
    };
    if final_period.kind != TokenKind::Period {
        errors.push(crate::ParseError {
            message: "syntax error: expected '.' after ENDIF".to_string(),
            range: endif_tok.range.start..final_period.range.end,
        });
        let recover = recover_skip_after_endif(source, tokens, endif_idx);
        let node = b.branch(
            SyntaxKind::IfStmt,
            if_tok.range.start..final_period.range.end,
            &children,
        );
        return Some((node, recover));
    }

    children.push(token_leaf(b, endif_tok));
    children.push(token_leaf(b, final_period));
    let node = b.branch(
        SyntaxKind::IfStmt,
        if_tok.range.start..final_period.range.end,
        &children,
    );
    Some((node, k + 1))
}

#[cfg(test)]
mod tests {
    use abap_ast::SyntaxKind;
    use abap_lexer::tokenize;

    use crate::parse;
    use crate::syntax::build_file_tree;

    fn build_ok(src: &str) -> abap_ast::File {
        let tok = tokenize(src);
        let mut err = Vec::new();
        let file = build_file_tree(src, &tok.tokens, src.len(), &mut err);
        assert!(err.is_empty(), "{:?}", err);
        file
    }

    #[test]
    fn if_only() {
        let src = "IF lv > 0. ENDIF.";
        let file = build_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::IfStmt), 1);
    }

    #[test]
    fn if_else_endif() {
        let src = "IF a = 1. lv = 2. ELSE. lv = 3. ENDIF.";
        let file = build_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::IfStmt), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::ElseClause), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::AssignStmt), 2);
    }

    #[test]
    fn if_elseif_else_nested() {
        let src = concat!(
            "IF x = 1. lv = 1. ELSEIF x = 2. IF y > 0. lv = 2. ENDIF. ",
            "ELSE. lv = 3. ENDIF.",
        );
        let file = build_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::IfStmt), 2);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::ElseifClause), 1);
    }

    #[test]
    fn else_pragma_before_period() {
        let src = "IF abap_true IS INITIAL.\nELSE ##EC_NO_CHECK .\nENDIF.";
        let tok = tokenize(src);
        let mut err = Vec::new();
        let file = build_file_tree(src, &tok.tokens, src.len(), &mut err);
        assert!(err.is_empty(), "{:?}", err);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::IfStmt), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::ElseClause), 1);
    }

    #[test]
    fn endif_pragma_before_period() {
        let src = "IF abap_false IS INITIAL. ENDIF ##NEEDED .";
        let tok = tokenize(src);
        let mut err = Vec::new();
        let file = build_file_tree(src, &tok.tokens, src.len(), &mut err);
        assert!(err.is_empty(), "{:?}", err);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::IfStmt), 1);
    }

    #[test]
    fn missing_period_after_if_condition_recovers_to_body_and_endif() {
        let src = "IF lv > 0\nlv = 1.\nENDIF.";
        let parsed = parse(src);
        assert!(
            parsed
                .errors
                .iter()
                .any(|e| e.message.contains("expected '.' after IF condition")),
            "{:?}",
            parsed.errors
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::IfStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::AssignStmt),
            1
        );
        assert!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::Error)
                >= 1
        );
    }

    #[test]
    fn missing_period_after_elseif_condition_recovers_to_following_arms() {
        let src = concat!(
            "IF a = 1. lv = 1. ",
            "ELSEIF b = 2\nlv = 2. ",
            "ELSE. lv = 3. ENDIF.",
        );
        let parsed = parse(src);
        assert!(
            parsed
                .errors
                .iter()
                .any(|e| e.message.contains("expected '.' after ELSEIF condition")),
            "{:?}",
            parsed.errors
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::IfStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::ElseifClause),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::ElseClause),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::AssignStmt),
            3
        );
    }

    #[test]
    fn malformed_else_still_recovers_to_after_endif() {
        let src = "IF a = 1. ELSE lv = 2. ENDIF. DATA lv TYPE i.";
        let parsed = parse(src);
        assert!(
            parsed
                .errors
                .iter()
                .any(|e| e.message.contains("expected '.' after ELSE")),
            "{:?}",
            parsed.errors
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::IfStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::DataDecl),
            1
        );
    }

    #[test]
    fn missing_endif_reports_error() {
        let parsed = parse("IF a = 1. lv = 1.");
        assert!(
            parsed
                .errors
                .iter()
                .any(|e| e.message.contains("expected ENDIF")),
            "{:?}",
            parsed.errors
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::IfStmt),
            1
        );
    }

    #[test]
    fn multiline_if_condition_continues_after_and() {
        let parsed = parse(
            "IF it_bup_reg_key IS INITIAL AND\n   ib_force_deletion = abap_true.\n  WRITE 'x'.\nENDIF.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::IfStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::WriteStmt),
            1
        );
    }

    #[test]
    fn multiline_if_condition_allows_keyword_named_operand_after_and() {
        let parsed = parse(
            "IF lv_ok = abap_true AND\n   FUNCTION NE 'DELE' AND FUNCTION NE 'SAVE'.\n  WRITE 'x'.\nENDIF.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::IfStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::WriteStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn multiline_elseif_condition_allows_keyword_named_operand_after_and() {
        let parsed = parse(
            "IF lv_ok = abap_true.\nELSEIF NOT lo_service IS INITIAL AND\n   FUNCTION NE 'DELE' AND FUNCTION NE 'SAVE'.\n  WRITE 'x'.\nENDIF.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::IfStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::WriteStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn multiline_if_condition_continues_after_not() {
        let parsed = parse("IF NOT\n   iv_flag = abap_true.\n  WRITE 'x'.\nENDIF.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::IfStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::WriteStmt),
            1
        );
    }
}
