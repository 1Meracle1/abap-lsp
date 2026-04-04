use abap_ast::SyntaxKind;
use abap_ast::arena::{NodeId, SyntaxTreeBuilder};
use abap_lexer::{Token, TokenKind};

use crate::block_helpers::{
    error_token_children, is_keyword, next_after_unterminated_scan, parse_body_until_keywords,
    parse_header_until_period, recover_skip_after_keyword, skip_trivia,
};
use crate::expr::parse_arithmetic_expr;
use crate::stmt_period::{
    StmtPeriodScan, is_definite_stmt_lead_keyword, scan_until_statement_period, token_begins_line,
    unterminated_err_end,
};
use crate::syntax::token_leaf;
use crate::type_ref::build_type_ref_node;

fn scan_until_top_level_period(tokens: &[Token], start: usize) -> Option<usize> {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut i = start;
    while i < tokens.len() {
        let t = &tokens[i];
        match t.kind {
            TokenKind::Eof => return None,
            TokenKind::Period if paren == 0 && bracket == 0 && brace == 0 => return Some(i),
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
    None
}

fn starts_hyphenated_keyword(tokens: &[Token], idx: usize) -> bool {
    tokens.get(idx + 1).map(|t| t.kind) == Some(TokenKind::Minus)
}

fn class_header_is_block(tokens: &[Token], source: &str, idx: usize) -> bool {
    let Some(period_i) = scan_until_top_level_period(tokens, idx + 1) else {
        return true;
    };
    for tok in &tokens[idx + 1..period_i] {
        if tok.kind == TokenKind::Ident
            && (tok.lexeme(source).eq_ignore_ascii_case("load")
                || tok.lexeme(source).eq_ignore_ascii_case("deferred"))
        {
            return false;
        }
    }
    true
}

fn select_header_is_flat(
    tokens: &[Token],
    source: &str,
    idx: usize,
    next_after_header: usize,
) -> bool {
    let mut i = idx + 1;
    let header_end = next_after_header.saturating_sub(1);
    while i < header_end {
        let tok = &tokens[i];
        if is_keyword(source, tok, "single") {
            return true;
        }
        if tok.kind == TokenKind::Ident
            && matches!(
                tok.lexeme(source).to_ascii_uppercase().as_str(),
                "COUNT" | "MAX" | "MIN" | "SUM" | "AVG"
            )
            && tokens.get(i + 1).map(|next| next.kind) == Some(TokenKind::LParen)
        {
            return true;
        }
        if is_keyword(source, tok, "into") || is_keyword(source, tok, "appending") {
            let mut j = skip_trivia(tokens, i + 1);
            if tokens.get(j).map(|next| next.kind) == Some(TokenKind::LParen) {
                return true;
            }
            if tokens
                .get(j)
                .is_some_and(|next| is_keyword(source, next, "corresponding"))
            {
                j = skip_trivia(tokens, j + 1);
                if tokens
                    .get(j)
                    .is_some_and(|next| is_keyword(source, next, "fields"))
                {
                    j = skip_trivia(tokens, j + 1);
                }
                if tokens
                    .get(j)
                    .is_some_and(|next| is_keyword(source, next, "of"))
                {
                    j = skip_trivia(tokens, j + 1);
                }
            }
            if tokens
                .get(j)
                .is_some_and(|next| is_keyword(source, next, "table"))
            {
                return true;
            }
        }
        i += 1;
    }
    false
}

fn scan_read_table_stmt_period(tokens: &[Token], source: &str, start: usize) -> StmtPeriodScan {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut inside_key_components = false;
    let mut i = start;

    while i < tokens.len() {
        let t = &tokens[i];
        if t.kind == TokenKind::Eof {
            return StmtPeriodScan::Unterminated { end_exclusive: i };
        }

        if paren == 0 && bracket == 0 && brace == 0 {
            if t.kind == TokenKind::Period {
                return StmtPeriodScan::Found(i);
            }

            if t.kind == TokenKind::Ident {
                if is_keyword(source, t, "with") {
                    let mut j = skip_trivia(tokens, i + 1);
                    if tokens
                        .get(j)
                        .is_some_and(|next| is_keyword(source, next, "table"))
                    {
                        j = skip_trivia(tokens, j + 1);
                    }
                    if tokens
                        .get(j)
                        .is_some_and(|next| is_keyword(source, next, "key"))
                    {
                        inside_key_components = true;
                    }
                } else if is_keyword(source, t, "into")
                    || is_keyword(source, t, "assigning")
                    || is_keyword(source, t, "transporting")
                    || is_keyword(source, t, "using")
                    || is_keyword(source, t, "binary")
                {
                    inside_key_components = false;
                }
            }

            if i > start && t.kind == TokenKind::Ident && token_begins_line(source, t) {
                if is_definite_stmt_lead_keyword(source, t) {
                    return StmtPeriodScan::Unterminated { end_exclusive: i };
                }
                if !inside_key_components {
                    let next_kind = tokens.get(i + 1).map(|x| x.kind);
                    if matches!(next_kind, Some(TokenKind::Eq | TokenKind::QuestionEq)) {
                        return StmtPeriodScan::Unterminated { end_exclusive: i };
                    }
                }
            }
        }

        match t.kind {
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

    StmtPeriodScan::Unterminated {
        end_exclusive: tokens.len(),
    }
}

fn parse_simple_keyword_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    kind: SyntaxKind,
    keyword: &str,
    errors: &mut Vec<crate::ParseError>,
    missing_period_message: &str,
) -> Option<(NodeId, usize)> {
    let tok = tokens.get(idx)?;
    if !is_keyword(source, tok, keyword) {
        return None;
    }
    match scan_until_statement_period(tokens, source, idx + 1) {
        StmtPeriodScan::Found(period_i) => {
            let mut children = Vec::with_capacity(period_i - idx + 1);
            for t in &tokens[idx..=period_i] {
                children.push(token_leaf(b, t));
            }
            let node = b.branch(kind, tok.range.start..tokens[period_i].range.end, &children);
            Some((node, period_i + 1))
        }
        StmtPeriodScan::Unterminated { end_exclusive } => {
            let err_end = unterminated_err_end(tokens, end_exclusive, tok.range.end);
            errors.push(crate::ParseError {
                message: missing_period_message.to_string(),
                range: tok.range.start..err_end,
            });
            let mut children = Vec::with_capacity(end_exclusive.saturating_sub(idx));
            for t in &tokens[idx..end_exclusive] {
                children.push(token_leaf(b, t));
            }
            let node = b.branch(SyntaxKind::Error, tok.range.start..err_end, &children);
            Some((
                node,
                if tokens.get(end_exclusive).map(|t| t.kind) == Some(TokenKind::Eof) {
                    tokens.len()
                } else {
                    end_exclusive
                },
            ))
        }
    }
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

fn try_parse_field_symbol_inline_decl(
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
    let node = b.branch(
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
    );
    Some((node, next_idx + 1))
}

fn push_token_children(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) {
    for token in &tokens[start..end_exclusive] {
        children.push(token_leaf(b, token));
    }
}

fn push_expr_child(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
    prev_before_first: Option<&Token>,
) {
    if start >= end_exclusive {
        return;
    }
    children.push(parse_arithmetic_expr(
        b,
        source,
        &tokens[start..end_exclusive],
        prev_before_first,
    ));
}

fn scan_until_clause(
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
    clause_starts: impl Fn(&[Token], usize) -> bool,
) -> usize {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut i = start;
    while i < end_exclusive {
        let token = &tokens[i];
        if paren == 0 && bracket == 0 && brace == 0 && clause_starts(tokens, i) {
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
        i += 1;
    }
    i
}

fn read_table_clause_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    token.kind == TokenKind::Ident
        && (is_keyword(source, token, "into")
            || is_keyword(source, token, "assigning")
            || is_keyword(source, token, "with")
            || is_keyword(source, token, "index")
            || is_keyword(source, token, "using")
            || is_keyword(source, token, "transporting")
            || is_keyword(source, token, "comparing")
            || is_keyword(source, token, "binary")
            || (is_keyword(source, token, "reference")
                && tokens
                    .get(idx + 1)
                    .is_some_and(|next| is_keyword(source, next, "into"))))
}

fn append_clause_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    token.kind == TokenKind::Ident
        && (is_keyword(source, token, "to")
            || is_keyword(source, token, "assigning")
            || is_keyword(source, token, "sorted")
            || (is_keyword(source, token, "reference")
                && tokens
                    .get(idx + 1)
                    .is_some_and(|next| is_keyword(source, next, "into"))))
}

fn scan_read_table_key_value_end(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> usize {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut i = start;
    while i < end_exclusive {
        let token = &tokens[i];
        if paren == 0 && bracket == 0 && brace == 0 {
            if read_table_clause_starts(source, tokens, i) {
                break;
            }
            if token.kind == TokenKind::Ident
                && tokens
                    .get(i + 1)
                    .is_some_and(|next| next.kind == TokenKind::Eq)
            {
                break;
            }
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
        i += 1;
    }
    i
}

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

fn try_parse_block_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    start_kw: &str,
    end_kw: &str,
    kind: SyntaxKind,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let start_tok = tokens.get(idx)?;
    if !is_keyword(source, start_tok, start_kw) {
        return None;
    }
    let (mut children, mut next) = parse_header_until_period(
        b,
        source,
        tokens,
        idx,
        idx + 1,
        errors,
        &format!("syntax error: expected '.' after {start_kw} header"),
    );
    let (body, after_body) = parse_body_until_keywords(b, source, tokens, next, errors, &[end_kw]);
    children.extend(body);
    next = after_body;
    let (end_children, next_after, end_pos) = parse_end_keyword(
        b,
        source,
        tokens,
        next,
        start_tok,
        end_kw,
        &format!("syntax error: expected {end_kw}"),
        errors,
    );
    children.extend(end_children);
    let node = b.branch(kind, start_tok.range.start..end_pos, &children);
    Some((node, next_after))
}

fn form_header_section_keyword(source: &str, token: &Token) -> bool {
    is_keyword(source, token, "tables")
        || is_keyword(source, token, "using")
        || is_keyword(source, token, "changing")
        || is_keyword(source, token, "raises")
}

fn form_header_starts_typed_param(source: &str, tokens: &[Token], idx: usize, end: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    if token.kind != TokenKind::Ident {
        return false;
    }
    let mut j = idx;
    if is_keyword(source, token, "value") || is_keyword(source, token, "reference") {
        if tokens.get(j + 1).map(|t| t.kind) != Some(TokenKind::LParen)
            || tokens.get(j + 2).map(|t| t.kind) != Some(TokenKind::Ident)
            || tokens.get(j + 3).map(|t| t.kind) != Some(TokenKind::RParen)
        {
            return false;
        }
        j += 4;
    } else {
        j += 1;
    }
    while j < end && tokens[j].kind == TokenKind::Comment {
        j += 1;
    }
    tokens
        .get(j)
        .is_some_and(|t| is_keyword(source, t, "type") || is_keyword(source, t, "like"))
}

fn skip_form_header_type_expression(
    source: &str,
    tokens: &[Token],
    mut idx: usize,
    end: usize,
) -> usize {
    let mut depth = 0i32;
    while idx < end {
        let token = &tokens[idx];
        match token.kind {
            TokenKind::Comment => idx += 1,
            TokenKind::LParen => {
                depth += 1;
                idx += 1;
            }
            TokenKind::RParen => {
                depth -= 1;
                idx += 1;
            }
            TokenKind::Period if depth == 0 => return idx,
            _ if depth == 0 && form_header_section_keyword(source, token) => return idx,
            _ if depth == 0 && form_header_starts_typed_param(source, tokens, idx, end) => {
                return idx;
            }
            _ => idx += 1,
        }
    }
    idx
}

fn form_header_type_ref_ranges(
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<(usize, usize)> {
    let mut ranges = Vec::new();
    let mut i = idx + 1;
    while i <= period_i && tokens[i].kind == TokenKind::Comment {
        i += 1;
    }
    if i > period_i || tokens[i].kind != TokenKind::Ident {
        return ranges;
    }
    i += 1;

    while i <= period_i {
        let token = &tokens[i];
        if token.kind == TokenKind::Comment {
            i += 1;
            continue;
        }
        if token.kind == TokenKind::Period {
            break;
        }
        if form_header_section_keyword(source, token) {
            i += 1;
            continue;
        }
        if !form_header_starts_typed_param(source, tokens, i, period_i + 1) {
            i += 1;
            continue;
        }
        let mut j = i;
        if is_keyword(source, &tokens[j], "value") || is_keyword(source, &tokens[j], "reference") {
            j += 4;
        } else {
            j += 1;
        }
        while j <= period_i && tokens[j].kind == TokenKind::Comment {
            j += 1;
        }
        if j > period_i {
            break;
        }
        j += 1;
        while j <= period_i && tokens[j].kind == TokenKind::Comment {
            j += 1;
        }
        let expr_start = j;
        let expr_end = skip_form_header_type_expression(source, tokens, expr_start, period_i + 1);
        if expr_start < expr_end {
            ranges.push((expr_start, expr_end));
        }
        i = expr_end;
    }
    ranges
}

fn build_form_header_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let ranges = form_header_type_ref_ranges(source, tokens, idx, period_i);
    if ranges.is_empty() {
        return tokens[idx..=period_i]
            .iter()
            .map(|t| token_leaf(b, t))
            .collect();
    }
    let mut children = Vec::with_capacity(period_i - idx + 1);
    let mut i = idx;
    let mut range_idx = 0usize;
    while i <= period_i {
        if let Some((start, end)) = ranges.get(range_idx).copied()
            && i == start
        {
            children.push(build_type_ref_node(b, source, &tokens[start..end]));
            i = end;
            range_idx += 1;
            continue;
        }
        children.push(token_leaf(b, &tokens[i]));
        i += 1;
    }
    children
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

pub fn try_parse_report_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    parse_simple_keyword_stmt(
        b,
        source,
        tokens,
        idx,
        SyntaxKind::ReportStmt,
        "report",
        errors,
        "syntax error: expected '.' after REPORT",
    )
}

pub fn try_parse_include_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    parse_simple_keyword_stmt(
        b,
        source,
        tokens,
        idx,
        SyntaxKind::IncludeStmt,
        "include",
        errors,
        "syntax error: expected '.' after INCLUDE",
    )
}

pub fn try_parse_write_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    parse_simple_keyword_stmt(
        b,
        source,
        tokens,
        idx,
        SyntaxKind::WriteStmt,
        "write",
        errors,
        "syntax error: expected '.' after WRITE statement",
    )
}

pub fn try_parse_concatenate_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    parse_simple_keyword_stmt(
        b,
        source,
        tokens,
        idx,
        SyntaxKind::ConcatenateStmt,
        "concatenate",
        errors,
        "syntax error: expected '.' after CONCATENATE statement",
    )
}

pub fn try_parse_raise_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    parse_simple_keyword_stmt(
        b,
        source,
        tokens,
        idx,
        SyntaxKind::RaiseStmt,
        "raise",
        errors,
        "syntax error: expected '.' after RAISE statement",
    )
}

pub fn try_parse_endat_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    parse_simple_keyword_stmt(
        b,
        source,
        tokens,
        idx,
        SyntaxKind::EndAtStmt,
        "endat",
        errors,
        "syntax error: expected '.' after ENDAT",
    )
}

pub fn try_parse_call_like_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let first = tokens.get(idx)?;
    let is_call_stmt = is_keyword(source, first, "call")
        && tokens.get(idx + 1).is_some_and(|t| {
            is_keyword(source, t, "method")
                || is_keyword(source, t, "function")
                || is_keyword(source, t, "transformation")
                || is_keyword(source, t, "badi")
        });
    let is_create_object = is_keyword(source, first, "create")
        && tokens
            .get(idx + 1)
            .is_some_and(|t| is_keyword(source, t, "object"));
    if !is_call_stmt && !is_create_object {
        return None;
    }

    if let Some(period_i) = scan_until_top_level_period(tokens, idx + 1) {
        let mut children = Vec::with_capacity(period_i - idx + 1);
        for t in &tokens[idx..=period_i] {
            children.push(token_leaf(b, t));
        }
        let kind = if is_create_object {
            SyntaxKind::CreateObjectStmt
        } else if tokens
            .get(idx + 1)
            .is_some_and(|token| is_keyword(source, token, "method"))
        {
            SyntaxKind::CallMethodStmt
        } else {
            SyntaxKind::CallStmt
        };
        let node = b.branch(
            kind,
            first.range.start..tokens[period_i].range.end,
            &children,
        );
        return Some((node, period_i + 1));
    }

    let err_end = tokens
        .iter()
        .rfind(|t| t.kind != TokenKind::Eof)
        .map(|t| t.range.end)
        .unwrap_or(first.range.end);
    errors.push(crate::ParseError {
        message: "syntax error: expected '.' to end call-like statement".to_string(),
        range: first.range.start..err_end,
    });
    let mut children = Vec::new();
    for t in &tokens[idx..] {
        if t.kind == TokenKind::Eof {
            break;
        }
        children.push(token_leaf(b, t));
    }
    let node = b.branch(SyntaxKind::Error, first.range.start..err_end, &children);
    Some((node, tokens.len()))
}

pub fn try_parse_read_table_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let read_tok = tokens.get(idx)?;
    if !is_keyword(source, read_tok, "read")
        || !tokens
            .get(idx + 1)
            .is_some_and(|t| is_keyword(source, t, "table"))
    {
        return None;
    }
    match scan_read_table_stmt_period(tokens, source, idx + 2) {
        StmtPeriodScan::Found(period_i) => {
            let mut children = Vec::with_capacity(period_i - idx + 1);
            children.push(token_leaf(b, read_tok));
            children.push(token_leaf(b, &tokens[idx + 1]));

            let mut i = idx + 2;
            let source_end = scan_until_clause(tokens, i, period_i, |tokens, idx| {
                read_table_clause_starts(source, tokens, idx)
            });
            push_expr_child(
                b,
                &mut children,
                source,
                tokens,
                i,
                source_end,
                tokens.get(idx + 1),
            );
            i = source_end;

            while i < period_i {
                let token = &tokens[i];
                if is_keyword(source, token, "into") {
                    children.push(token_leaf(b, token));
                    let target_end = scan_until_clause(tokens, i + 1, period_i, |tokens, idx| {
                        read_table_clause_starts(source, tokens, idx)
                    });
                    push_expr_child(
                        b,
                        &mut children,
                        source,
                        tokens,
                        i + 1,
                        target_end,
                        Some(token),
                    );
                    i = target_end;
                    continue;
                }
                if is_keyword(source, token, "assigning") {
                    children.push(token_leaf(b, token));
                    if let Some((inline_decl, next_i)) =
                        try_parse_field_symbol_inline_decl(b, source, tokens, i + 1)
                    {
                        children.push(inline_decl);
                        i = next_i;
                        continue;
                    }
                    let target_end = scan_until_clause(tokens, i + 1, period_i, |tokens, idx| {
                        read_table_clause_starts(source, tokens, idx)
                    });
                    push_expr_child(
                        b,
                        &mut children,
                        source,
                        tokens,
                        i + 1,
                        target_end,
                        Some(token),
                    );
                    i = target_end;
                    continue;
                }
                if is_keyword(source, token, "reference")
                    && tokens
                        .get(i + 1)
                        .is_some_and(|next| is_keyword(source, next, "into"))
                {
                    children.push(token_leaf(b, token));
                    children.push(token_leaf(b, &tokens[i + 1]));
                    let target_end = scan_until_clause(tokens, i + 2, period_i, |tokens, idx| {
                        read_table_clause_starts(source, tokens, idx)
                    });
                    push_expr_child(
                        b,
                        &mut children,
                        source,
                        tokens,
                        i + 2,
                        target_end,
                        Some(&tokens[i + 1]),
                    );
                    i = target_end;
                    continue;
                }
                if is_keyword(source, token, "index") {
                    children.push(token_leaf(b, token));
                    let expr_end = scan_until_clause(tokens, i + 1, period_i, |tokens, idx| {
                        read_table_clause_starts(source, tokens, idx)
                    });
                    push_expr_child(
                        b,
                        &mut children,
                        source,
                        tokens,
                        i + 1,
                        expr_end,
                        Some(token),
                    );
                    i = expr_end;
                    continue;
                }
                if is_keyword(source, token, "with") {
                    children.push(token_leaf(b, token));
                    i += 1;
                    while i < period_i {
                        let current = &tokens[i];
                        if read_table_clause_starts(source, tokens, i) {
                            break;
                        }
                        children.push(token_leaf(b, current));
                        if current.kind == TokenKind::Eq {
                            let value_end =
                                scan_read_table_key_value_end(source, tokens, i + 1, period_i);
                            push_expr_child(
                                b,
                                &mut children,
                                source,
                                tokens,
                                i + 1,
                                value_end,
                                Some(current),
                            );
                            i = value_end;
                            continue;
                        }
                        i += 1;
                    }
                    continue;
                }
                children.push(token_leaf(b, token));
                i += 1;
            }
            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::ReadTableStmt,
                read_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            Some((node, period_i + 1))
        }
        StmtPeriodScan::Unterminated { end_exclusive } => {
            let err_end = unterminated_err_end(tokens, end_exclusive, read_tok.range.end);
            errors.push(crate::ParseError {
                message: "syntax error: expected '.' after READ TABLE statement".to_string(),
                range: read_tok.range.start..err_end,
            });
            let mut children = Vec::with_capacity(end_exclusive.saturating_sub(idx));
            for t in &tokens[idx..end_exclusive] {
                children.push(token_leaf(b, t));
            }
            let node = b.branch(SyntaxKind::Error, read_tok.range.start..err_end, &children);
            Some((node, end_exclusive))
        }
    }
}

pub fn try_parse_append_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let append_tok = tokens.get(idx)?;
    if !is_keyword(source, append_tok, "append") {
        return None;
    }
    match scan_until_statement_period(tokens, source, idx + 1) {
        StmtPeriodScan::Found(period_i) => {
            let Some(to_idx) = (idx + 1..period_i).find(|&i| is_keyword(source, &tokens[i], "to"))
            else {
                return None;
            };
            let mut children = Vec::with_capacity(period_i - idx + 1);
            children.push(token_leaf(b, append_tok));

            let source_end = scan_until_clause(tokens, idx + 1, to_idx, |tokens, idx| {
                append_clause_starts(source, tokens, idx)
            });
            push_expr_child(
                b,
                &mut children,
                source,
                tokens,
                idx + 1,
                source_end,
                Some(append_tok),
            );
            push_token_children(b, &mut children, tokens, source_end, to_idx);

            children.push(token_leaf(b, &tokens[to_idx]));
            let mut i = to_idx + 1;
            let target_end = scan_until_clause(tokens, i, period_i, |tokens, idx| {
                append_clause_starts(source, tokens, idx)
            });
            push_expr_child(
                b,
                &mut children,
                source,
                tokens,
                i,
                target_end,
                Some(&tokens[to_idx]),
            );
            i = target_end;

            while i < period_i {
                let token = &tokens[i];
                if is_keyword(source, token, "assigning") {
                    children.push(token_leaf(b, token));
                    if let Some((inline_decl, next_i)) =
                        try_parse_field_symbol_inline_decl(b, source, tokens, i + 1)
                    {
                        children.push(inline_decl);
                        i = next_i;
                        continue;
                    }
                } else if is_keyword(source, token, "reference")
                    && tokens
                        .get(i + 1)
                        .is_some_and(|next| is_keyword(source, next, "into"))
                {
                    children.push(token_leaf(b, token));
                    children.push(token_leaf(b, &tokens[i + 1]));
                    let expr_end = scan_until_clause(tokens, i + 2, period_i, |tokens, idx| {
                        append_clause_starts(source, tokens, idx)
                    });
                    push_expr_child(
                        b,
                        &mut children,
                        source,
                        tokens,
                        i + 2,
                        expr_end,
                        Some(&tokens[i + 1]),
                    );
                    i = expr_end;
                    continue;
                } else if is_keyword(source, token, "sorted") {
                    children.push(token_leaf(b, token));
                    let expr_end = scan_until_clause(tokens, i + 1, period_i, |tokens, idx| {
                        append_clause_starts(source, tokens, idx)
                    });
                    push_expr_child(
                        b,
                        &mut children,
                        source,
                        tokens,
                        i + 1,
                        expr_end,
                        Some(token),
                    );
                    i = expr_end;
                    continue;
                }
                children.push(token_leaf(b, token));
                i += 1;
            }
            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::AppendStmt,
                append_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            Some((node, period_i + 1))
        }
        StmtPeriodScan::Unterminated { end_exclusive } => {
            let err_end = unterminated_err_end(tokens, end_exclusive, append_tok.range.end);
            errors.push(crate::ParseError {
                message: "syntax error: expected '.' after APPEND statement".to_string(),
                range: append_tok.range.start..err_end,
            });
            let mut children = Vec::with_capacity(end_exclusive.saturating_sub(idx));
            for t in &tokens[idx..end_exclusive] {
                children.push(token_leaf(b, t));
            }
            let node = b.branch(
                SyntaxKind::Error,
                append_tok.range.start..err_end,
                &children,
            );
            Some((node, end_exclusive))
        }
    }
}

pub fn try_parse_assign_keyword_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let assign_tok = tokens.get(idx)?;
    if !is_keyword(source, assign_tok, "assign") {
        return None;
    }

    match scan_until_statement_period(tokens, source, idx + 1) {
        StmtPeriodScan::Found(period_i) => {
            let mut children = Vec::with_capacity(period_i - idx + 1);
            let mut i = idx;
            while i < period_i {
                if let Some((inline_decl, next_i)) =
                    try_parse_field_symbol_inline_decl(b, source, tokens, i)
                {
                    children.push(inline_decl);
                    i = next_i;
                    continue;
                }
                children.push(token_leaf(b, &tokens[i]));
                i += 1;
            }
            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::AssignKeywordStmt,
                assign_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            Some((node, period_i + 1))
        }
        StmtPeriodScan::Unterminated { end_exclusive } => {
            let err_end = unterminated_err_end(tokens, end_exclusive, assign_tok.range.end);
            errors.push(crate::ParseError {
                message: "syntax error: expected '.' after ASSIGN statement".to_string(),
                range: assign_tok.range.start..err_end,
            });
            let mut children = Vec::with_capacity(end_exclusive.saturating_sub(idx));
            for t in &tokens[idx..end_exclusive] {
                children.push(token_leaf(b, t));
            }
            let node = b.branch(
                SyntaxKind::Error,
                assign_tok.range.start..err_end,
                &children,
            );
            Some((node, end_exclusive))
        }
    }
}

pub fn try_parse_event_block(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let start_tok = tokens.get(idx)?;
    if start_tok.kind != TokenKind::Ident {
        return None;
    }
    let body_start_idx = if is_keyword(source, start_tok, "initialization") {
        idx + 1
    } else if let Some(next) =
        match_hyphenated_keyword(source, tokens, idx, &["start", "of", "selection"])
    {
        next
    } else if let Some(next) =
        match_hyphenated_keyword(source, tokens, idx, &["end", "of", "selection"])
    {
        next
    } else if let Some(next) = match_hyphenated_keyword(source, tokens, idx, &["top", "of", "page"])
    {
        next
    } else if let Some(next) = match_hyphenated_keyword(source, tokens, idx, &["end", "of", "page"])
    {
        next
    } else {
        return None;
    };

    let (mut children, mut next) = parse_header_until_period(
        b,
        source,
        tokens,
        idx,
        body_start_idx,
        errors,
        "syntax error: expected '.' after event block header",
    );
    let (body, after_body) = parse_body_until_keywords(
        b,
        source,
        tokens,
        next,
        errors,
        &[
            "START",
            "END",
            "INITIALIZATION",
            "TOP",
            "REPORT",
            "INCLUDE",
            "FORM",
            "MODULE",
            "CLASS",
            "INTERFACE",
        ],
    );
    children.extend(body);
    next = after_body;
    let end = children
        .last()
        .copied()
        .map(|id| b.span(id).end)
        .unwrap_or(start_tok.range.end);
    let node = b.branch(
        SyntaxKind::EventBlock,
        start_tok.range.start..end,
        &children,
    );
    Some((node, next))
}

pub fn try_parse_form_decl(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let start_tok = tokens.get(idx)?;
    if !is_keyword(source, start_tok, "form") {
        return None;
    }
    let (mut children, mut next) = match scan_until_statement_period(tokens, source, idx + 1) {
        StmtPeriodScan::Found(period_i) => (
            build_form_header_children(b, source, tokens, idx, period_i),
            period_i + 1,
        ),
        StmtPeriodScan::Unterminated { end_exclusive } => {
            let err_end = unterminated_err_end(tokens, end_exclusive, start_tok.range.end);
            errors.push(crate::ParseError {
                message: "syntax error: expected '.' after form header".to_string(),
                range: start_tok.range.start..err_end,
            });
            let err_children = error_token_children(b, tokens, idx, end_exclusive);
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
    };
    let (body, after_body) =
        parse_body_until_keywords(b, source, tokens, next, errors, &["ENDFORM"]);
    children.extend(body);
    next = after_body;
    let (end_children, next_after, end_pos) = parse_end_keyword(
        b,
        source,
        tokens,
        next,
        start_tok,
        "ENDFORM",
        "syntax error: expected ENDFORM",
        errors,
    );
    children.extend(end_children);
    let node = b.branch(
        SyntaxKind::FormDecl,
        start_tok.range.start..end_pos,
        &children,
    );
    Some((node, next_after))
}

pub fn try_parse_module_decl(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    if starts_hyphenated_keyword(tokens, idx) {
        return None;
    }
    try_parse_block_stmt(
        b,
        source,
        tokens,
        idx,
        "module",
        "ENDMODULE",
        SyntaxKind::ModuleDecl,
        errors,
    )
}

pub fn try_parse_class_decl(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    if starts_hyphenated_keyword(tokens, idx) {
        return None;
    }
    if !class_header_is_block(tokens, source, idx) {
        return None;
    }
    try_parse_block_stmt(
        b,
        source,
        tokens,
        idx,
        "class",
        "ENDCLASS",
        SyntaxKind::ClassDecl,
        errors,
    )
}

pub fn try_parse_interface_decl(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    if starts_hyphenated_keyword(tokens, idx) {
        return None;
    }
    try_parse_block_stmt(
        b,
        source,
        tokens,
        idx,
        "interface",
        "ENDINTERFACE",
        SyntaxKind::InterfaceDecl,
        errors,
    )
}

pub fn try_parse_method_decl(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    try_parse_block_stmt(
        b,
        source,
        tokens,
        idx,
        "method",
        "ENDMETHOD",
        SyntaxKind::MethodDecl,
        errors,
    )
}

pub fn try_parse_select_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let select_tok = tokens.get(idx)?;
    if !is_keyword(source, select_tok, "select") {
        return None;
    }

    let (mut children, next) = parse_header_until_period(
        b,
        source,
        tokens,
        idx,
        idx + 1,
        errors,
        "syntax error: expected '.' after SELECT statement",
    );

    let mut cursor = next;
    let endselect_idx = recover_skip_after_keyword(source, tokens, next, "ENDSELECT");
    if !select_header_is_flat(tokens, source, idx, next) && endselect_idx != tokens.len() {
        let (body, after_body) =
            parse_body_until_keywords(b, source, tokens, cursor, errors, &["ENDSELECT"]);
        children.extend(body);
        cursor = after_body;
        let (end_children, next_after, end_pos) = parse_end_keyword(
            b,
            source,
            tokens,
            cursor,
            select_tok,
            "ENDSELECT",
            "syntax error: expected ENDSELECT",
            errors,
        );
        children.extend(end_children);
        let node = b.branch(
            SyntaxKind::SelectStmt,
            select_tok.range.start..end_pos,
            &children,
        );
        return Some((node, next_after));
    }

    let end = children
        .last()
        .copied()
        .map(|id| b.span(id).end)
        .unwrap_or(select_tok.range.end);
    let node = b.branch(
        SyntaxKind::SelectStmt,
        select_tok.range.start..end,
        &children,
    );
    Some((node, cursor))
}

#[cfg(test)]
mod tests {
    use abap_ast::SyntaxKind;

    #[test]
    fn parses_form_body() {
        let parsed = crate::parse("FORM run. DATA lv TYPE i. ENDFORM.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::FormDecl),
            1
        );
    }

    #[test]
    fn parses_form_header_type_refs_structurally() {
        let parsed =
            crate::parse("FORM run USING VALUE(iv_row) TYPE REF TO zif_demo=>ty_row. ENDFORM.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let form = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::FormDecl)
            .expect("form");
        assert_eq!(parsed.file.count_kind(form, SyntaxKind::TypeRefSimple), 2);
        assert_eq!(
            parsed
                .file
                .count_kind(form, SyntaxKind::TypeRefSelectorChain),
            1
        );
    }

    #[test]
    fn parses_class_method_impl() {
        let parsed =
            crate::parse("CLASS lcl IMPLEMENTATION. METHOD run. WRITE 'x'. ENDMETHOD. ENDCLASS.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::ClassDecl),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::MethodDecl),
            1
        );
    }

    #[test]
    fn parses_select_endselect_block() {
        let parsed = crate::parse("SELECT * FROM t INTO wa. WRITE wa. ENDSELECT.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::SelectStmt),
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
    fn parses_flat_select_into_table_without_endselect() {
        let parsed = crate::parse("SELECT * FROM t INTO TABLE lt_rows. WRITE 'x'.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::SelectStmt),
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
    fn parses_concatenate_stmt() {
        let parsed = crate::parse(
            "CONCATENATE 'Document' mv_odlv INTO lv_delivery_msg SEPARATED BY ': '.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::ConcatenateStmt),
            1
        );
    }

    #[test]
    fn parses_flat_select_into_tuple_without_endselect() {
        let parsed = crate::parse(
            "SELECT MAX( bup_role_variant ) COUNT( * ) INTO ( lv_max, lv_count ) FROM demo. IF lv_count > 0. ENDIF.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::SelectStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::IfStmt),
            1
        );
    }

    #[test]
    fn parses_flat_select_count_into_scalar_without_endselect() {
        let parsed = crate::parse(
            "SELECT COUNT( * ) FROM demo INTO lv_count WHERE key = value. IF lv_count > 0. ENDIF.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::SelectStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::IfStmt),
            1
        );
    }

    #[test]
    fn parses_multiline_call_method_with_named_args_as_one_statement() {
        let parsed = crate::parse(
            "CALL METHOD zcl_demo=>run\n  EXPORTING\n    iv_a = lv_a\n  IMPORTING\n    ev_b = lv_b.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::CallMethodStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::AssignStmt),
            0
        );
    }

    #[test]
    fn parses_legacy_call_method_with_inline_importing_targets_as_one_statement() {
        let parsed = crate::parse(
            "CALL METHOD zcl_demo=>get_event_data\n  EXPORTING\n    iv_evtid = mv_evtid\n  IMPORTING\n    es_evt = DATA(ls_evt)\n    et_items = DATA(lt_items).",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::CallMethodStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::AssignStmt),
            0
        );
    }

    #[test]
    fn parses_create_object_with_exporting_clause_as_one_statement() {
        let parsed = crate::parse("CREATE OBJECT lo_client\n  EXPORTING\n    iv_dest = lv_dest.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::CreateObjectStmt),
            1
        );
    }

    #[test]
    fn class_hyphenated_decls_do_not_start_nested_class_blocks() {
        let parsed = crate::parse(
            "CLASS lcl DEFINITION.\n  PUBLIC SECTION.\n    CLASS-DATA gv_value TYPE i.\n    CLASS-METHODS run\n      IMPORTING\n        iv_x TYPE i\n      EXPORTING\n        ev_y TYPE i.\nENDCLASS.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::ClassDecl),
            1
        );
    }

    #[test]
    fn parses_multiline_read_table_with_key_into_inline_data() {
        let parsed = crate::parse(
            "READ TABLE lt_obj_hier_upd\n  WITH KEY gs1_es = lv_epc\n  INTO DATA(ls_ser_par).",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::ReadTableStmt),
            1
        );
    }

    #[test]
    fn parses_multiline_read_table_with_assigning_field_symbol() {
        let parsed = crate::parse(
            "READ TABLE lt_unpack_lvls\n  WITH KEY parent = ls_ser_par-gs1_es_parent\n  ASSIGNING FIELD-SYMBOL(<fs_unpack_data>).",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::ReadTableStmt),
            1
        );
    }

    #[test]
    fn parses_multiline_read_table_transporting_no_fields() {
        let parsed = crate::parse(
            "READ TABLE <fs_unpack_data>-children\n  WITH KEY table_line = lv_epc\n  TRANSPORTING NO FIELDS.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::ReadTableStmt),
            1
        );
    }

    #[test]
    fn parses_read_table_index_using_key_assigning() {
        let parsed = crate::parse(
            "READ TABLE itab INDEX idx USING KEY sort_key ASSIGNING FIELD-SYMBOL(<fs>).",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::ReadTableStmt),
            1
        );
    }

    #[test]
    fn parses_read_table_operands_as_ast_children() {
        let parsed = crate::parse("READ TABLE lt_trn INTO ls_trn INDEX 1.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::ReadTableStmt)
            .expect("read table stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::TemplateExpr), 3);
    }

    #[test]
    fn parses_append_operands_as_ast_children() {
        let parsed = crate::parse("APPEND ls_evt TO lt_evt.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::AppendStmt)
            .expect("append stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::TemplateExpr), 2);
    }

    #[test]
    fn parses_assign_to_inline_field_symbol() {
        let parsed = crate::parse("ASSIGN mo_outbound->* TO FIELD-SYMBOL(<ls_outbound>).");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::AssignKeywordStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::FieldSymbolInlineDecl),
            1
        );
    }

    #[test]
    fn parses_assign_component_to_inline_field_symbol() {
        let parsed = crate::parse(
            "ASSIGN COMPONENT 'EPCISDOCUMENT-EPCISBODY-EVENT_LIST-CHOICE'\n  OF STRUCTURE <ls_outbound>\n  TO FIELD-SYMBOL(<ls_event>) ##no_text.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::AssignKeywordStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::FieldSymbolInlineDecl),
            1
        );
    }

    #[test]
    fn parses_select_where_condition_split_after_and() {
        let parsed = crate::parse(
            "SELECT *\n  APPENDING CORRESPONDING FIELDS OF TABLE lt_rows\n  FROM demo\n  WHERE bupid = ls_key-bupid AND\n        regid = ls_key-regid.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::SelectStmt),
            1
        );
    }

    #[test]
    fn parses_direct_static_method_call_with_named_args_as_call_stmt() {
        let parsed = crate::parse(
            "cl_abap_message_digest=>calculate_hash_for_char(\n  EXPORTING\n    if_algorithm = lv_algorithm\n    if_data      = lv_data\n  IMPORTING\n    ef_hashstring = lv_hashstring\n).",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::CallStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::AssignStmt),
            0
        );
    }

    #[test]
    fn parses_direct_static_method_call_with_inline_importing_data_targets() {
        let parsed = crate::parse(
            "zcl_demo=>get_event_data(\n  EXPORTING\n    iv_evtid = mv_evtid\n  IMPORTING\n    es_evt = DATA(ls_evt)\n    et_items = DATA(lt_items)\n).",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::CallStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::AssignStmt),
            0
        );
    }

    #[test]
    fn parses_raise_exception_type_with_exporting_named_args() {
        let parsed = crate::parse(
            "RAISE EXCEPTION TYPE /sttp/cx_base_exception\n  EXPORTING\n    message_text = gv_dummy_msg\n    returncode   = /sttp/cl_constants=>gcs_rc-fail.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::RaiseStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::AssignStmt),
            0
        );
    }

    #[test]
    fn parses_endat_as_dedicated_stmt() {
        let parsed = crate::parse("ENDAT.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::EndAtStmt),
            1
        );
    }

    #[test]
    fn parses_call_function_as_dedicated_stmt() {
        let parsed = crate::parse("CALL FUNCTION 'RFC_PING'.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::CallStmt),
            1
        );
    }

    #[test]
    fn parses_direct_instance_method_call_as_call_stmt() {
        let parsed = crate::parse("lo_handler->run( iv_mode = lv_mode ).");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::CallStmt),
            1
        );
    }

    #[test]
    fn class_definition_load_does_not_start_class_block() {
        let parsed = crate::parse(
            "CLASS lcl DEFINITION.\n  PUBLIC SECTION.\n    CLASS cl_demo DEFINITION LOAD.\nENDCLASS.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::ClassDecl),
            1
        );
    }
}
