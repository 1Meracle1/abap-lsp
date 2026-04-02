//! Simple statements that are not structured further yet: a run of tokens up to a top-level `.`.

use abap_ast::arena::{NodeId, SyntaxTreeBuilder};
use abap_ast::SyntaxKind;
use abap_lexer::{Token, TokenKind};

use crate::stmt_period::{scan_until_statement_period, unterminated_err_end, StmtPeriodScan};

fn token_leaf(b: &mut SyntaxTreeBuilder, token: &Token) -> NodeId {
    b.leaf(SyntaxKind::Token, token.range.clone())
}

/// `keyword token* .` for any leading identifier after dedicated parsers (`DATA`, `IF`, …) have
/// declined.
pub fn try_parse_simple_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let first = tokens.get(idx)?;
    if first.kind != TokenKind::Ident {
        return None;
    }

    match scan_until_statement_period(tokens, source, idx) {
        StmtPeriodScan::Found(period_i) => {
            let period_tok = &tokens[period_i];
            let mut kids = Vec::with_capacity(period_i - idx + 1);
            for t in &tokens[idx..=period_i] {
                kids.push(token_leaf(b, t));
            }
            let node = b.branch(
                SyntaxKind::SimpleStmt,
                first.range.start..period_tok.range.end,
                &kids,
            );
            Some((node, period_i + 1))
        }
        StmtPeriodScan::Unterminated { end_exclusive } => {
            let err_end = unterminated_err_end(tokens, end_exclusive, first.range.end);
            errors.push(crate::ParseError {
                message: "syntax error: expected '.' to end statement".to_string(),
                range: first.range.start..err_end,
            });
            let mut kids = Vec::with_capacity(end_exclusive.saturating_sub(idx));
            for t in &tokens[idx..end_exclusive] {
                kids.push(token_leaf(b, t));
            }
            let node = b.branch(
                SyntaxKind::Error,
                first.range.start..err_end,
                &kids,
            );
            let next = if tokens.get(end_exclusive).map(|t| t.kind) == Some(TokenKind::Eof) {
                tokens.len()
            } else {
                end_exclusive
            };
            Some((node, next))
        }
    }
}
