use abap_ast::SyntaxKind;
use abap_ast::arena::{NodeId, SyntaxTreeBuilder};
use abap_lexer::{Token, TokenKind, have_space_between};

use crate::block_helpers::{
    error_token_children, is_keyword, next_after_unterminated_scan, parse_body_until_keywords,
    parse_header_until_period, recover_skip_after_keyword, skip_trivia,
};
use crate::expr::{parse_arithmetic_expr, parse_logical_expr};
use crate::stmt_period::{
    StmtPeriodScan, is_condition_continuation_keyword, is_definite_stmt_lead_keyword,
    is_inline_decl_continuation, is_named_arg_clause_keyword,
    line_start_condition_operand_continues, line_start_named_arg_continues,
    line_start_table_key_component_continues, scan_until_statement_period,
    scan_until_statement_period_with_named_args, starts_with_table_key_clause, token_begins_line,
    unterminated_err_end,
};
use crate::syntax::token_leaf;
use crate::type_ref::{build_type_ref_node, parse_type_ref_tokens};

#[derive(Clone, Copy)]
enum EventBlockLead {
    Single(&'static str),
    Hyphenated(&'static [&'static str]),
    AtHyphenated(&'static [&'static str]),
}

const EVENT_BLOCK_LEADS: &[EventBlockLead] = &[
    EventBlockLead::Single("initialization"),
    EventBlockLead::Hyphenated(&["start", "of", "selection"]),
    EventBlockLead::Hyphenated(&["end", "of", "selection"]),
    EventBlockLead::Hyphenated(&["top", "of", "page"]),
    EventBlockLead::Hyphenated(&["end", "of", "page"]),
    EventBlockLead::AtHyphenated(&["selection", "screen"]),
    EventBlockLead::AtHyphenated(&["line", "selection"]),
];

const EVENT_BLOCK_BODY_BOUNDARY_KEYWORDS: &[&str] = &[
    "START",
    "END",
    "AT",
    "INITIALIZATION",
    "TOP",
    "REPORT",
    "INCLUDE",
    "FORM",
    "MODULE",
    "CLASS",
    "INTERFACE",
];

const GET_TIME_STAMP_FIELD_LEAD: &[&str] = &["get", "time", "stamp", "field"];
const GET_REFERENCE_OF_LEAD: &[&str] = &["get", "reference", "of"];
const MACRO_END_OF_DEFINITION: &[&str] = &["end", "of", "definition"];

#[derive(Clone, Copy, PartialEq, Eq)]
enum CallLikeLeadKind {
    CallMethod,
    CallStmt,
    SystemFunctionCall,
    CreateObject,
    CreateData,
}

const CALL_LIKE_LEADS: &[(&[&str], CallLikeLeadKind)] = &[
    (&["call", "method"], CallLikeLeadKind::CallMethod),
    (&["call", "function"], CallLikeLeadKind::CallStmt),
    (&["call", "transformation"], CallLikeLeadKind::CallStmt),
    (&["call", "badi"], CallLikeLeadKind::CallStmt),
    (&["call", "screen"], CallLikeLeadKind::CallStmt),
    (&["call", "transaction"], CallLikeLeadKind::CallStmt),
    (&["create", "object"], CallLikeLeadKind::CreateObject),
    (&["create", "data"], CallLikeLeadKind::CreateData),
];

const NON_SYSTEM_CALL_VARIANTS: &[&str] = &[
    "badi",
    "customer",
    "database",
    "dialog",
    "function",
    "method",
    "screen",
    "selection",
    "subscreen",
    "transaction",
    "transformation",
];

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

fn scan_until_selection_screen_period(
    tokens: &[Token],
    source: &str,
    start: usize,
) -> StmtPeriodScan {
    if let Some(period_i) = scan_until_top_level_period(tokens, start) {
        let has_chain_colon = tokens[start..period_i]
            .iter()
            .any(|token| token.kind == TokenKind::Colon);
        if has_chain_colon {
            return StmtPeriodScan::Found(period_i);
        }
    }
    scan_until_statement_period(tokens, source, start)
}

fn parse_selection_screen_stmt_with_period_scan<F>(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    scan_start: usize,
    start_tok: &Token,
    missing_period_message: &str,
    errors: &mut Vec<crate::ParseError>,
    on_found: F,
) -> (NodeId, usize)
where
    F: FnOnce(&mut SyntaxTreeBuilder, usize, &mut Vec<crate::ParseError>) -> (NodeId, usize),
{
    match scan_until_selection_screen_period(tokens, source, scan_start) {
        StmtPeriodScan::Found(period_i) => on_found(b, period_i, errors),
        StmtPeriodScan::Unterminated { end_exclusive } => {
            let err_end = unterminated_err_end(tokens, end_exclusive, start_tok.range.end);
            errors.push(crate::ParseError {
                message: missing_period_message.to_string(),
                range: start_tok.range.start..err_end,
            });
            let children = token_children(b, tokens, idx, end_exclusive);
            let node = b.branch(SyntaxKind::Error, start_tok.range.start..err_end, &children);
            (node, next_after_unterminated_scan(tokens, end_exclusive))
        }
    }
}

fn call_method_clause_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    token.kind == TokenKind::Ident
        && (is_keyword(source, token, "exporting")
            || is_keyword(source, token, "importing")
            || is_keyword(source, token, "changing")
            || is_keyword(source, token, "receiving")
            || is_keyword(source, token, "exceptions"))
}

fn call_inner_padding_is_valid(tokens: &[Token], lparen_idx: usize, rparen_idx: usize) -> bool {
    let lparen = &tokens[lparen_idx];
    let rparen = &tokens[rparen_idx];
    let mut first = None;
    let mut last = None;
    for token in &tokens[lparen_idx + 1..rparen_idx] {
        if token.kind == TokenKind::Comment {
            continue;
        }
        if first.is_none() {
            first = Some(token);
        }
        last = Some(token);
    }
    match (first, last) {
        (Some(first), Some(_)) => have_space_between(lparen, first),
        _ => have_space_between(lparen, rparen),
    }
}

fn dynamic_selector_lparen(tokens: &[Token], lparen_idx: usize) -> bool {
    if tokens.get(lparen_idx).map(|token| token.kind) != Some(TokenKind::LParen) {
        return false;
    }
    let Some(prev) = lparen_idx.checked_sub(1).and_then(|idx| tokens.get(idx)) else {
        return false;
    };
    matches!(prev.kind, TokenKind::Arrow | TokenKind::FatArrow)
        && !have_space_between(prev, &tokens[lparen_idx])
}

fn validate_call_method_inline_args_spacing(
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> bool {
    if call_like_lead_kind(source, tokens, idx) != Some((CallLikeLeadKind::CallMethod, idx + 2)) {
        return true;
    }

    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut lparen_idx = None;
    for i in idx + 2..period_i {
        let token = &tokens[i];
        if paren == 0 && bracket == 0 && brace == 0 && call_method_clause_starts(source, tokens, i)
        {
            break;
        }
        match token.kind {
            TokenKind::LParen if paren == 0 && bracket == 0 && brace == 0 => {
                if i > idx + 2
                    && !dynamic_selector_lparen(tokens, i)
                    && !have_space_between(&tokens[i - 1], token)
                {
                    lparen_idx = Some(i);
                }
                paren += 1;
            }
            TokenKind::LParen => paren += 1,
            TokenKind::RParen => {
                paren -= 1;
                if paren == 0
                    && bracket == 0
                    && brace == 0
                    && let Some(lparen_idx) = lparen_idx
                {
                    return call_inner_padding_is_valid(tokens, lparen_idx, i);
                }
            }
            TokenKind::LBracket => bracket += 1,
            TokenKind::RBracket => bracket -= 1,
            TokenKind::LBrace => brace += 1,
            TokenKind::RBrace => brace -= 1,
            _ => {}
        }
    }

    true
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

fn interface_header_is_block(tokens: &[Token], source: &str, idx: usize) -> bool {
    let Some(period_i) = scan_until_top_level_period(tokens, idx + 1) else {
        return true;
    };
    let significant = tokens[idx..=period_i]
        .iter()
        .filter(|token| token.kind != TokenKind::Comment)
        .collect::<Vec<_>>();
    if significant
        .last()
        .map(|token| token.kind != TokenKind::Period)
        .unwrap_or(true)
        || significant
            .first()
            .map(|token| !is_keyword(source, token, "interface"))
            .unwrap_or(true)
    {
        return true;
    }

    let mut i = 1usize;
    let mut saw_entry = false;
    let mut saw_deferred_or_load = false;
    loop {
        while matches!(
            significant.get(i).map(|token| token.kind),
            Some(TokenKind::Colon | TokenKind::Comma)
        ) {
            i += 1;
        }
        if significant
            .get(i)
            .is_some_and(|token| token.kind == TokenKind::Period)
        {
            break;
        }
        if significant.get(i).map(|token| token.kind) != Some(TokenKind::Ident) {
            return true;
        }
        i += 1;
        let Some(next) = significant.get(i) else {
            return true;
        };
        if is_keyword(source, next, "deferred") || is_keyword(source, next, "load") {
            saw_deferred_or_load = true;
            saw_entry = true;
            i += 1;
            continue;
        }
        return true;
    }
    if saw_entry && saw_deferred_or_load {
        return false;
    }
    true
}

fn select_header_is_flat(
    tokens: &[Token],
    source: &str,
    idx: usize,
    next_after_header: usize,
) -> bool {
    let header_end = next_after_header.saturating_sub(1);
    if select_header_has_top_level_keyword_sequence(
        source,
        tokens,
        idx + 1,
        header_end,
        &["single"],
    ) {
        return true;
    }
    if select_header_has_top_level_keyword_sequence(
        source,
        tokens,
        idx + 1,
        header_end,
        &["package", "size"],
    ) {
        return false;
    }
    if select_header_has_top_level_set_operator(source, tokens, idx + 1, header_end) {
        return false;
    }
    if select_header_has_top_level_keyword_sequence(
        source,
        tokens,
        idx + 1,
        header_end,
        &["group", "by"],
    ) {
        return false;
    }
    if select_header_has_into_table_target(source, tokens, idx + 1, header_end) {
        return true;
    }
    select_header_projection_is_aggregate_only(source, tokens, idx + 1, header_end)
}

fn token_keyword_sequence_matches(
    source: &str,
    tokens: &[Token],
    idx: usize,
    parts: &[&str],
) -> bool {
    match_keyword_sequence(source, tokens, idx, parts).is_some()
}

fn select_header_top_level_positions<F>(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
    mut predicate: F,
) -> bool
where
    F: FnMut(&[Token], usize) -> bool,
{
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut sql_case_depth = 0i32;
    let mut idx = start;
    while idx < end_exclusive {
        let token = &tokens[idx];
        if paren == 0 && bracket == 0 && brace == 0 && sql_case_depth == 0 && predicate(tokens, idx)
        {
            return true;
        }
        match token.kind {
            TokenKind::LParen => paren += 1,
            TokenKind::RParen => paren -= 1,
            TokenKind::LBracket => bracket += 1,
            TokenKind::RBracket => bracket -= 1,
            TokenKind::LBrace => brace += 1,
            TokenKind::RBrace => brace -= 1,
            TokenKind::Ident if is_keyword(source, token, "case") => sql_case_depth += 1,
            TokenKind::Ident if is_keyword(source, token, "end") && sql_case_depth > 0 => {
                sql_case_depth -= 1
            }
            _ => {}
        }
        idx += 1;
    }
    false
}

fn select_header_has_top_level_keyword_sequence(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
    parts: &[&str],
) -> bool {
    select_header_top_level_positions(source, tokens, start, end_exclusive, |tokens, idx| {
        token_keyword_sequence_matches(source, tokens, idx, parts)
    })
}

fn select_set_operator_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    token.kind == TokenKind::Ident
        && (is_keyword(source, token, "union")
            || is_keyword(source, token, "intersect")
            || is_keyword(source, token, "except"))
}

fn select_header_has_top_level_set_operator(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> bool {
    select_header_top_level_positions(source, tokens, start, end_exclusive, |tokens, idx| {
        select_set_operator_starts(source, tokens, idx)
    })
}

fn select_header_has_into_table_target(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> bool {
    select_header_top_level_positions(source, tokens, start, end_exclusive, |tokens, idx| {
        let Some(token) = tokens.get(idx) else {
            return false;
        };
        if !(is_keyword(source, token, "into") || is_keyword(source, token, "appending")) {
            return false;
        }
        let mut target_idx = advance_select_target_prefix(source, tokens, idx + 1);
        if tokens.get(target_idx).map(|token| token.kind) == Some(TokenKind::At) {
            target_idx = skip_trivia(tokens, target_idx + 1);
        }
        tokens
            .get(skip_trivia(tokens, idx + 1))
            .is_some_and(|next| is_keyword(source, next, "table"))
            || tokens
                .get(target_idx.saturating_sub(1))
                .is_some_and(|prev| is_keyword(source, prev, "table"))
    })
}

fn select_header_projection_bounds(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> Option<(usize, usize)> {
    let mut cursor = skip_trivia(tokens, start);
    if tokens
        .get(cursor)
        .is_some_and(|token| is_keyword(source, token, "single"))
    {
        cursor = skip_trivia(tokens, cursor + 1);
        if token_keyword_sequence_matches(source, tokens, cursor, &["for", "update"]) {
            cursor = skip_trivia(tokens, cursor + 2);
        }
    }
    if tokens
        .get(cursor)
        .is_some_and(|token| is_keyword(source, token, "distinct"))
    {
        cursor = skip_trivia(tokens, cursor + 1);
    }

    if tokens
        .get(cursor)
        .is_some_and(|token| is_keyword(source, token, "from"))
    {
        let fields_idx =
            find_top_level_keyword(source, tokens, cursor + 1, end_exclusive, "fields")?;
        let projection_start = skip_trivia(tokens, fields_idx + 1);
        let projection_end =
            scan_until_clause(tokens, projection_start, end_exclusive, |tokens, idx| {
                select_clause_start_kind(source, tokens, idx)
                    .is_some_and(|kind| kind != SelectClauseKind::Fields)
            });
        return (projection_start < projection_end).then_some((projection_start, projection_end));
    }

    let projection_end = scan_until_clause(tokens, cursor, end_exclusive, |tokens, idx| {
        select_clause_start_kind(source, tokens, idx).is_some()
    });
    (cursor < projection_end).then_some((cursor, projection_end))
}

fn sql_token_is_aggregate_name(source: &str, token: &Token) -> bool {
    token.kind == TokenKind::Ident
        && matches!(
            token.lexeme(source).to_ascii_uppercase().as_str(),
            "COUNT"
                | "MAX"
                | "MIN"
                | "SUM"
                | "AVG"
                | "MEDIAN"
                | "STDDEV"
                | "VAR"
                | "CORR"
                | "CORR_SPEARMAN"
                | "ALLOW_PRECISION_LOSS"
        )
}

fn skip_projection_alias_after_aggregate(
    source: &str,
    tokens: &[Token],
    mut idx: usize,
    end_exclusive: usize,
) -> usize {
    idx = skip_trivia(tokens, idx);
    if tokens
        .get(idx)
        .is_some_and(|token| is_keyword(source, token, "as"))
    {
        let alias_idx = skip_trivia(tokens, idx + 1);
        if tokens
            .get(alias_idx)
            .is_some_and(|token| token.kind == TokenKind::Ident)
        {
            return skip_trivia(tokens, alias_idx + 1);
        }
        return idx;
    }
    if idx < end_exclusive
        && tokens[idx].kind == TokenKind::Ident
        && !sql_token_is_aggregate_name(source, &tokens[idx])
        && !sql_token_is_keyword(source, &tokens[idx])
    {
        return skip_trivia(tokens, idx + 1);
    }
    idx
}

fn select_header_projection_is_aggregate_only(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> bool {
    let Some((projection_start, projection_end)) =
        select_header_projection_bounds(source, tokens, start, end_exclusive)
    else {
        return false;
    };
    let mut idx = projection_start;
    let mut saw_aggregate = false;
    while idx < projection_end {
        idx = skip_trivia(tokens, idx);
        while idx < projection_end && tokens[idx].kind == TokenKind::Comma {
            idx = skip_trivia(tokens, idx + 1);
        }
        if idx >= projection_end {
            break;
        }
        if !sql_token_is_aggregate_name(source, &tokens[idx])
            || tokens
                .get(skip_trivia(tokens, idx + 1))
                .map(|token| token.kind)
                != Some(TokenKind::LParen)
        {
            return false;
        }
        let lparen_idx = skip_trivia(tokens, idx + 1);
        let Some(close_idx) = find_matching_delim_in_range(
            tokens,
            lparen_idx,
            projection_end,
            TokenKind::LParen,
            TokenKind::RParen,
        ) else {
            return false;
        };
        saw_aggregate = true;
        idx = skip_projection_alias_after_aggregate(source, tokens, close_idx + 1, projection_end);
        if idx < projection_end && tokens[idx].kind != TokenKind::Comma {
            if sql_token_is_aggregate_name(source, &tokens[idx])
                && tokens
                    .get(skip_trivia(tokens, idx + 1))
                    .map(|token| token.kind)
                    == Some(TokenKind::LParen)
            {
                continue;
            }
            return false;
        }
    }
    saw_aggregate
}

fn select_target_clause_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    token.kind == TokenKind::Ident
        && (is_keyword(source, token, "where")
            || is_keyword(source, token, "having")
            || is_keyword(source, token, "group")
            || is_keyword(source, token, "order")
            || is_keyword(source, token, "package")
            || is_keyword(source, token, "bypassing")
            || is_keyword(source, token, "connection")
            || is_keyword(source, token, "client")
            || is_keyword(source, token, "privileged")
            || is_keyword(source, token, "up")
            || is_keyword(source, token, "union")
            || is_keyword(source, token, "intersect")
            || is_keyword(source, token, "except")
            || is_keyword(source, token, "for")
            || is_keyword(source, token, "offset"))
}

fn advance_select_target_prefix(source: &str, tokens: &[Token], start: usize) -> usize {
    let mut idx = skip_trivia(tokens, start);
    if tokens
        .get(idx)
        .is_some_and(|token| is_keyword(source, token, "corresponding"))
    {
        idx = skip_trivia(tokens, idx + 1);
        if tokens
            .get(idx)
            .is_some_and(|token| is_keyword(source, token, "fields"))
        {
            idx = skip_trivia(tokens, idx + 1);
        }
        if tokens
            .get(idx)
            .is_some_and(|token| is_keyword(source, token, "of"))
        {
            idx = skip_trivia(tokens, idx + 1);
        }
    }
    if tokens
        .get(idx)
        .is_some_and(|token| is_keyword(source, token, "table"))
    {
        idx = skip_trivia(tokens, idx + 1);
    }
    idx
}

fn push_select_target_clause_children(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    source: &str,
    tokens: &[Token],
    clause_idx: usize,
    period_i: usize,
) -> usize {
    let target_prefix_end = advance_select_target_prefix(source, tokens, clause_idx + 1);
    let mut expr_start = target_prefix_end;
    if tokens.get(expr_start).map(|token| token.kind) == Some(TokenKind::At) {
        expr_start += 1;
    }
    push_token_children(b, children, tokens, clause_idx, expr_start);
    let target_end = scan_until_clause(tokens, expr_start, period_i, |tokens, idx| {
        select_target_clause_starts(source, tokens, idx)
    });
    if expr_start < target_end {
        let inline_end = trim_trailing_comment_tokens(tokens, expr_start, target_end);
        if let Some((inline_decl, next_idx)) =
            try_parse_data_inline_decl(b, source, tokens, expr_start)
            && next_idx == inline_end
        {
            children.push(inline_decl);
            push_token_children(b, children, tokens, inline_end, target_end);
        } else if let Some((inline_decl, next_idx)) =
            try_parse_field_symbol_inline_decl(b, source, tokens, expr_start)
            && next_idx == inline_end
        {
            children.push(inline_decl);
            push_token_children(b, children, tokens, inline_end, target_end);
        } else if push_parenthesized_select_target_list_children(
            b, children, source, tokens, expr_start, target_end,
        ) {
        } else {
            push_expr_child(
                b,
                children,
                source,
                tokens,
                expr_start,
                target_end,
                tokens.get(expr_start.saturating_sub(1)),
            );
        }
    }
    target_end
}

fn push_parenthesized_select_target_list_children(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> bool {
    if start >= end_exclusive
        || tokens.get(start).map(|token| token.kind) != Some(TokenKind::LParen)
    {
        return false;
    }
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut close_idx = None;
    for idx in start..end_exclusive {
        match tokens[idx].kind {
            TokenKind::LParen => paren += 1,
            TokenKind::RParen => {
                paren -= 1;
                if paren == 0 && bracket == 0 && brace == 0 {
                    close_idx = Some(idx);
                    break;
                }
            }
            TokenKind::LBracket => bracket += 1,
            TokenKind::RBracket => bracket -= 1,
            TokenKind::LBrace => brace += 1,
            TokenKind::RBrace => brace -= 1,
            _ => {}
        }
    }
    let Some(close_idx) = close_idx else {
        return false;
    };
    if close_idx + 1 != end_exclusive {
        return false;
    }

    children.push(token_leaf(b, &tokens[start]));
    let mut item_start = skip_trivia(tokens, start + 1);
    let mut inner_paren = 0i32;
    let mut inner_bracket = 0i32;
    let mut inner_brace = 0i32;
    let mut idx = item_start;
    while idx < close_idx {
        match tokens[idx].kind {
            TokenKind::LParen => inner_paren += 1,
            TokenKind::RParen => inner_paren -= 1,
            TokenKind::LBracket => inner_bracket += 1,
            TokenKind::RBracket => inner_bracket -= 1,
            TokenKind::LBrace => inner_brace += 1,
            TokenKind::RBrace => inner_brace -= 1,
            TokenKind::Comma if inner_paren == 0 && inner_bracket == 0 && inner_brace == 0 => {
                let item_end = trim_trailing_comment_tokens(tokens, item_start, idx);
                push_expr_child(
                    b,
                    children,
                    source,
                    tokens,
                    item_start,
                    item_end,
                    tokens.get(item_start.saturating_sub(1)),
                );
                children.push(token_leaf(b, &tokens[idx]));
                item_start = skip_trivia(tokens, idx + 1);
            }
            _ => {}
        }
        idx += 1;
    }

    let item_end = trim_trailing_comment_tokens(tokens, item_start, close_idx);
    push_expr_child(
        b,
        children,
        source,
        tokens,
        item_start,
        item_end,
        tokens.get(item_start.saturating_sub(1)),
    );
    children.push(token_leaf(b, &tokens[close_idx]));
    true
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum SelectClauseKind {
    Distinct,
    Fields,
    UpTo,
    PackageSize,
    Offset,
    AbapOptions,
    SetOperator,
    From,
    Into,
    Appending,
    Where,
    GroupBy,
    Having,
    OrderBy,
    ForAllEntries,
    ForUpdate,
}

fn select_clause_start_kind(
    source: &str,
    tokens: &[Token],
    idx: usize,
) -> Option<SelectClauseKind> {
    let token = tokens.get(idx)?;
    if token.kind != TokenKind::Ident {
        return None;
    }
    if is_keyword(source, token, "distinct") {
        return Some(SelectClauseKind::Distinct);
    }
    if is_keyword(source, token, "fields") {
        let prev_keyword_idx =
            tokens[..idx]
                .iter()
                .enumerate()
                .rev()
                .find_map(|(prev_idx, token)| {
                    (token.kind == TokenKind::Ident
                        && (is_keyword(source, token, "corresponding")
                            || is_keyword(source, token, "from")
                            || is_keyword(source, token, "single")
                            || is_keyword(source, token, "distinct")
                            || is_keyword(source, token, "to")
                            || is_keyword(source, token, "into")
                            || is_keyword(source, token, "appending")
                            || is_keyword(source, token, "where")
                            || is_keyword(source, token, "having")
                            || is_keyword(source, token, "group")
                            || is_keyword(source, token, "order")
                            || is_keyword(source, token, "for")))
                    .then_some(prev_idx)
                });
        let Some(prev_keyword_idx) = prev_keyword_idx else {
            return None;
        };
        let prev = &tokens[prev_keyword_idx];
        if is_keyword(source, prev, "corresponding") {
            return None;
        }
        if is_keyword(source, prev, "from")
            || is_keyword(source, prev, "single")
            || is_keyword(source, prev, "distinct")
            || (is_keyword(source, prev, "to")
                && tokens[..prev_keyword_idx]
                    .iter()
                    .enumerate()
                    .rev()
                    .find_map(|(candidate_idx, token)| {
                        (token.kind == TokenKind::Ident).then_some(candidate_idx)
                    })
                    .and_then(|candidate_idx| tokens.get(candidate_idx))
                    .is_some_and(|candidate| is_keyword(source, candidate, "up")))
        {
            return Some(SelectClauseKind::Fields);
        }
        return None;
    }
    if is_keyword(source, token, "from") {
        return Some(SelectClauseKind::From);
    }
    if is_keyword(source, token, "into") {
        return Some(SelectClauseKind::Into);
    }
    if is_keyword(source, token, "appending") {
        return Some(SelectClauseKind::Appending);
    }
    if is_keyword(source, token, "where") {
        return Some(SelectClauseKind::Where);
    }
    if is_keyword(source, token, "having") {
        return Some(SelectClauseKind::Having);
    }
    if is_keyword(source, token, "up")
        && tokens
            .get(skip_trivia(tokens, idx + 1))
            .is_some_and(|next| is_keyword(source, next, "to"))
    {
        return Some(SelectClauseKind::UpTo);
    }
    if is_keyword(source, token, "package")
        && tokens
            .get(skip_trivia(tokens, idx + 1))
            .is_some_and(|next| is_keyword(source, next, "size"))
    {
        return Some(SelectClauseKind::PackageSize);
    }
    if is_keyword(source, token, "offset") {
        return Some(SelectClauseKind::Offset);
    }
    let previous_is_with = previous_non_comment_token(tokens, idx)
        .and_then(|prev_idx| tokens.get(prev_idx))
        .is_some_and(|prev| is_keyword(source, prev, "with"));
    if (is_keyword(source, token, "bypassing")
        && tokens
            .get(skip_trivia(tokens, idx + 1))
            .is_some_and(|next| is_keyword(source, next, "buffer")))
        || is_keyword(source, token, "connection")
        || (is_keyword(source, token, "client")
            && tokens
                .get(skip_trivia(tokens, idx + 1))
                .is_some_and(|next| is_keyword(source, next, "specified")))
        || (!previous_is_with
            && is_keyword(source, token, "privileged")
            && tokens
                .get(skip_trivia(tokens, idx + 1))
                .is_some_and(|next| is_keyword(source, next, "access")))
    {
        return Some(SelectClauseKind::AbapOptions);
    }
    if select_set_operator_starts(source, tokens, idx) {
        return Some(SelectClauseKind::SetOperator);
    }
    if is_keyword(source, token, "group")
        && tokens
            .get(skip_trivia(tokens, idx + 1))
            .is_some_and(|next| is_keyword(source, next, "by"))
    {
        return Some(SelectClauseKind::GroupBy);
    }
    if is_keyword(source, token, "order")
        && tokens
            .get(skip_trivia(tokens, idx + 1))
            .is_some_and(|next| is_keyword(source, next, "by"))
    {
        return Some(SelectClauseKind::OrderBy);
    }
    if is_keyword(source, token, "for") {
        let all_idx = skip_trivia(tokens, idx + 1);
        let entries_idx = skip_trivia(tokens, all_idx + 1);
        let in_idx = skip_trivia(tokens, entries_idx + 1);
        if tokens
            .get(all_idx)
            .is_some_and(|next| is_keyword(source, next, "all"))
            && tokens
                .get(entries_idx)
                .is_some_and(|next| is_keyword(source, next, "entries"))
            && tokens
                .get(in_idx)
                .is_some_and(|next| is_keyword(source, next, "in"))
        {
            return Some(SelectClauseKind::ForAllEntries);
        }
        if tokens
            .get(all_idx)
            .is_some_and(|next| is_keyword(source, next, "update"))
        {
            return Some(SelectClauseKind::ForUpdate);
        }
    }
    None
}

fn build_token_branch(
    b: &mut SyntaxTreeBuilder,
    kind: SyntaxKind,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> Option<NodeId> {
    if start >= end_exclusive {
        return None;
    }
    let mut children = Vec::with_capacity(end_exclusive.saturating_sub(start));
    push_token_children(b, &mut children, tokens, start, end_exclusive);
    let range = tokens[start].range.start..tokens[end_exclusive - 1].range.end;
    Some(b.branch(kind, range, &children))
}

fn select_join_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    if token.kind != TokenKind::Ident {
        return false;
    }
    is_keyword(source, token, "join")
        || ((is_keyword(source, token, "inner")
            || is_keyword(source, token, "left")
            || is_keyword(source, token, "right")
            || is_keyword(source, token, "cross"))
            && tokens
                .get(skip_trivia(tokens, idx + 1))
                .is_some_and(|next| is_keyword(source, next, "join")))
}

fn find_top_level_keyword(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
    keyword: &str,
) -> Option<usize> {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut idx = start;
    while idx < end_exclusive {
        let token = &tokens[idx];
        if paren == 0
            && bracket == 0
            && brace == 0
            && token.kind == TokenKind::Ident
            && is_keyword(source, token, keyword)
        {
            return Some(idx);
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
    None
}

fn find_top_level_keyword_in(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
    keywords: &[&str],
) -> Option<usize> {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut idx = start;
    while idx < end_exclusive {
        let token = &tokens[idx];
        if paren == 0
            && bracket == 0
            && brace == 0
            && token.kind == TokenKind::Ident
            && keywords
                .iter()
                .any(|keyword| is_keyword(source, token, keyword))
        {
            return Some(idx);
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
    None
}

fn find_top_level_alias_as(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> Option<usize> {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut sql_case_depth = 0i32;
    let mut idx = start;
    while idx < end_exclusive {
        let token = &tokens[idx];
        if paren == 0
            && bracket == 0
            && brace == 0
            && sql_case_depth == 0
            && token.kind == TokenKind::Ident
            && is_keyword(source, token, "as")
        {
            let alias_idx = skip_trivia(tokens, idx + 1);
            if tokens
                .get(alias_idx)
                .is_some_and(|alias| alias.kind == TokenKind::Ident)
            {
                return Some(idx);
            }
        }
        match token.kind {
            TokenKind::LParen => paren += 1,
            TokenKind::RParen => paren -= 1,
            TokenKind::LBracket => bracket += 1,
            TokenKind::RBracket => bracket -= 1,
            TokenKind::LBrace => brace += 1,
            TokenKind::RBrace => brace -= 1,
            TokenKind::Ident if is_keyword(source, token, "case") => sql_case_depth += 1,
            TokenKind::Ident if is_keyword(source, token, "end") && sql_case_depth > 0 => {
                sql_case_depth -= 1
            }
            _ => {}
        }
        idx += 1;
    }
    None
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum SqlExprMode {
    Structured,
    Predicate,
}

fn sql_token_text_is_keyword(text: &str) -> bool {
    matches!(
        text.to_ascii_lowercase().as_str(),
        "select"
            | "single"
            | "distinct"
            | "case"
            | "when"
            | "then"
            | "else"
            | "end"
            | "from"
            | "into"
            | "appending"
            | "where"
            | "with"
            | "group"
            | "by"
            | "having"
            | "order"
            | "for"
            | "update"
            | "all"
            | "entries"
            | "in"
            | "up"
            | "to"
            | "rows"
            | "package"
            | "size"
            | "offset"
            | "bypassing"
            | "buffer"
            | "connection"
            | "client"
            | "specified"
            | "privileged"
            | "access"
            | "union"
            | "intersect"
            | "except"
            | "as"
            | "join"
            | "inner"
            | "left"
            | "right"
            | "cross"
            | "on"
            | "and"
            | "or"
            | "not"
            | "like"
            | "between"
            | "is"
            | "null"
            | "table"
            | "corresponding"
            | "fields"
            | "of"
            | "primary"
            | "key"
            | "exists"
    )
}

fn sql_token_is_keyword(source: &str, token: &Token) -> bool {
    token.kind == TokenKind::Ident && sql_token_text_is_keyword(token.lexeme(source))
}

fn sql_token_is_aggregate(source: &str, token: &Token) -> bool {
    token.kind == TokenKind::Ident
        && matches!(
            token.lexeme(source).to_ascii_uppercase().as_str(),
            "COUNT" | "MAX" | "MIN" | "SUM" | "AVG"
        )
}

fn find_matching_delim_in_range(
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
    open: TokenKind,
    close: TokenKind,
) -> Option<usize> {
    let end = find_matching_delim(tokens, start, open, close)?;
    (end < end_exclusive).then_some(end)
}

fn build_sql_alias_clause(
    b: &mut SyntaxTreeBuilder,
    tokens: &[Token],
    as_idx: usize,
    end_exclusive: usize,
) -> Option<NodeId> {
    if as_idx >= end_exclusive {
        return None;
    }
    let alias_idx = skip_trivia(tokens, as_idx + 1);
    if alias_idx >= end_exclusive {
        return None;
    }
    let mut children = vec![token_leaf(b, &tokens[as_idx])];
    if let Some(alias_node) =
        build_token_branch(b, SyntaxKind::SqlAlias, tokens, alias_idx, alias_idx + 1)
    {
        children.push(alias_node);
    }
    push_token_children(b, &mut children, tokens, alias_idx + 1, end_exclusive);
    let range = tokens[as_idx].range.start..tokens[end_exclusive - 1].range.end;
    Some(b.branch(SyntaxKind::SqlAliasClause, range, &children))
}

fn sql_host_expr_end_tokens(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> usize {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut idx = start;
    while idx < end_exclusive {
        let token = &tokens[idx];
        if paren == 0 && bracket == 0 && brace == 0 {
            if matches!(
                token.kind,
                TokenKind::Comma
                    | TokenKind::Period
                    | TokenKind::Eq
                    | TokenKind::Lt
                    | TokenKind::Gt
                    | TokenKind::Le
                    | TokenKind::Ge
                    | TokenKind::Ne
                    | TokenKind::QuestionEq
            ) || token.kind == TokenKind::RParen
                || sql_token_is_keyword(source, token)
            {
                break;
            }
        }
        match token.kind {
            TokenKind::LParen => paren += 1,
            TokenKind::RParen => {
                if paren == 0 {
                    break;
                }
                paren -= 1;
            }
            TokenKind::LBracket => bracket += 1,
            TokenKind::RBracket => {
                if bracket == 0 {
                    break;
                }
                bracket -= 1;
            }
            TokenKind::LBrace => brace += 1,
            TokenKind::RBrace => {
                if brace == 0 {
                    break;
                }
                brace -= 1;
            }
            _ => {}
        }
        idx += 1;
    }
    idx
}

fn build_sql_host_expr(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> Option<(NodeId, usize)> {
    if start >= end_exclusive || tokens[start].kind != TokenKind::At {
        return None;
    }
    let expr_end = sql_host_expr_end_tokens(source, tokens, start + 1, end_exclusive);
    if expr_end <= start + 1 {
        return None;
    }
    let mut children = Vec::new();
    push_token_children(b, &mut children, tokens, start, expr_end);
    let range = tokens[start].range.start..tokens[expr_end - 1].range.end;
    Some((
        b.branch(SyntaxKind::SqlHostExpr, range, &children),
        expr_end,
    ))
}

fn build_sql_aggregate_call(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> Option<(NodeId, usize)> {
    if start + 1 >= end_exclusive || !sql_token_is_aggregate(source, &tokens[start]) {
        return None;
    }
    if tokens[start + 1].kind != TokenKind::LParen {
        return None;
    }
    let end_idx = find_matching_delim_in_range(
        tokens,
        start + 1,
        end_exclusive,
        TokenKind::LParen,
        TokenKind::RParen,
    )?;
    let mut children = vec![
        token_leaf(b, &tokens[start]),
        token_leaf(b, &tokens[start + 1]),
    ];
    if start + 2 < end_idx {
        push_sql_expr_children(
            b,
            &mut children,
            source,
            tokens,
            start + 2,
            end_idx,
            SqlExprMode::Structured,
        );
    }
    children.push(token_leaf(b, &tokens[end_idx]));
    let range = tokens[start].range.start..tokens[end_idx].range.end;
    Some((
        b.branch(SyntaxKind::SqlAggregateCall, range, &children),
        end_idx + 1,
    ))
}

fn build_sql_paren_group(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
    mode: SqlExprMode,
) -> Option<(NodeId, usize)> {
    if start >= end_exclusive || tokens[start].kind != TokenKind::LParen {
        return None;
    }
    let end_idx = find_matching_delim_in_range(
        tokens,
        start,
        end_exclusive,
        TokenKind::LParen,
        TokenKind::RParen,
    )?;
    let mut children = vec![token_leaf(b, &tokens[start])];
    if start + 1 < end_idx {
        match mode {
            SqlExprMode::Structured => push_sql_expr_children(
                b,
                &mut children,
                source,
                tokens,
                start + 1,
                end_idx,
                SqlExprMode::Structured,
            ),
            SqlExprMode::Predicate => {
                push_sql_predicate_children(b, &mut children, source, tokens, start + 1, end_idx)
            }
        }
    }
    children.push(token_leaf(b, &tokens[end_idx]));
    let range = tokens[start].range.start..tokens[end_idx].range.end;
    Some((
        b.branch(SyntaxKind::SqlParenGroup, range, &children),
        end_idx + 1,
    ))
}

fn push_sql_expr_children(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
    mode: SqlExprMode,
) {
    let mut idx = start;
    while idx < end_exclusive {
        let token = &tokens[idx];
        if token.kind == TokenKind::Comment {
            children.push(token_leaf(b, token));
            idx += 1;
            continue;
        }
        if let Some((host_expr, next_idx)) =
            build_sql_host_expr(b, source, tokens, idx, end_exclusive)
        {
            children.push(host_expr);
            idx = next_idx;
            continue;
        }
        if let Some((paren_group, next_idx)) =
            build_sql_paren_group(b, source, tokens, idx, end_exclusive, mode)
        {
            children.push(paren_group);
            idx = next_idx;
            continue;
        }
        if let Some((aggregate, next_idx)) =
            build_sql_aggregate_call(b, source, tokens, idx, end_exclusive)
        {
            children.push(aggregate);
            idx = next_idx;
            continue;
        }
        if token.kind == TokenKind::Star {
            if let Some(node) = build_token_branch(b, SyntaxKind::SqlStar, tokens, idx, idx + 1) {
                children.push(node);
                idx += 1;
                continue;
            }
        }
        if idx + 2 < end_exclusive
            && token.kind == TokenKind::Ident
            && tokens[idx + 1].kind == TokenKind::Tilde
        {
            if tokens[idx + 2].kind == TokenKind::Star {
                if let Some(node) =
                    build_token_branch(b, SyntaxKind::SqlQualifiedStar, tokens, idx, idx + 3)
                {
                    children.push(node);
                    idx += 3;
                    continue;
                }
            }
            if tokens[idx + 2].kind == TokenKind::Ident {
                if let Some(node) =
                    build_token_branch(b, SyntaxKind::SqlQualifiedColumnRef, tokens, idx, idx + 3)
                {
                    children.push(node);
                    idx += 3;
                    continue;
                }
            }
        }
        if mode == SqlExprMode::Structured
            && token.kind == TokenKind::Ident
            && !sql_token_is_keyword(source, token)
        {
            if let Some(node) =
                build_token_branch(b, SyntaxKind::SqlColumnRef, tokens, idx, idx + 1)
            {
                children.push(node);
                idx += 1;
                continue;
            }
        }
        children.push(token_leaf(b, token));
        idx += 1;
    }
}

fn sql_predicate_token_is_separator(source: &str, token: &Token) -> bool {
    matches!(
        token.kind,
        TokenKind::Eq
            | TokenKind::Lt
            | TokenKind::Gt
            | TokenKind::Le
            | TokenKind::Ge
            | TokenKind::Ne
            | TokenKind::QuestionEq
            | TokenKind::Comma
            | TokenKind::Period
    ) || sql_token_is_keyword(source, token)
}

fn sql_predicate_operand_end(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> usize {
    if start >= end_exclusive {
        return start;
    }
    if tokens[start].kind == TokenKind::LParen
        && let Some(end_idx) = find_matching_delim_in_range(
            tokens,
            start,
            end_exclusive,
            TokenKind::LParen,
            TokenKind::RParen,
        )
    {
        return end_idx + 1;
    }
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut idx = start;
    while idx < end_exclusive {
        let token = &tokens[idx];
        if idx > start
            && paren == 0
            && bracket == 0
            && brace == 0
            && (sql_predicate_token_is_separator(source, token) || token.kind == TokenKind::RParen)
        {
            break;
        }
        match token.kind {
            TokenKind::LParen => paren += 1,
            TokenKind::RParen => {
                if paren == 0 {
                    break;
                }
                paren -= 1;
            }
            TokenKind::LBracket => bracket += 1,
            TokenKind::RBracket => {
                if bracket == 0 {
                    break;
                }
                bracket -= 1;
            }
            TokenKind::LBrace => brace += 1,
            TokenKind::RBrace => {
                if brace == 0 {
                    break;
                }
                brace -= 1;
            }
            _ => {}
        }
        idx += 1;
    }
    idx
}

fn build_sql_predicate_operand(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> Option<NodeId> {
    if start >= end_exclusive {
        return None;
    }
    let mut children = Vec::new();
    if let Some((paren_group, next_idx)) = build_sql_paren_group(
        b,
        source,
        tokens,
        start,
        end_exclusive,
        SqlExprMode::Predicate,
    ) && next_idx == end_exclusive
    {
        children.push(paren_group);
    } else {
        push_sql_expr_children(
            b,
            &mut children,
            source,
            tokens,
            start,
            end_exclusive,
            SqlExprMode::Predicate,
        );
    }
    let range = tokens[start].range.start..tokens[end_exclusive - 1].range.end;
    Some(b.branch(SyntaxKind::SqlPredicateOperand, range, &children))
}

fn push_sql_predicate_children(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) {
    let mut idx = start;
    while idx < end_exclusive {
        let token = &tokens[idx];
        if token.kind == TokenKind::Comment {
            children.push(token_leaf(b, token));
            idx += 1;
            continue;
        }
        if sql_predicate_token_is_separator(source, token) {
            children.push(token_leaf(b, token));
            idx += 1;
            continue;
        }
        let operand_end = sql_predicate_operand_end(source, tokens, idx, end_exclusive);
        if operand_end > idx
            && let Some(operand) = build_sql_predicate_operand(b, source, tokens, idx, operand_end)
        {
            children.push(operand);
            idx = operand_end;
            continue;
        }
        children.push(token_leaf(b, token));
        idx += 1;
    }
}

fn build_sql_predicate_expr(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> Option<NodeId> {
    if start >= end_exclusive {
        return None;
    }
    let mut children = Vec::new();
    push_sql_predicate_children(b, &mut children, source, tokens, start, end_exclusive);
    let range = tokens[start].range.start..tokens[end_exclusive - 1].range.end;
    Some(b.branch(SyntaxKind::SqlPredicateExpr, range, &children))
}

fn build_sql_data_source(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> Option<NodeId> {
    if start >= end_exclusive {
        return None;
    }
    let mut children = Vec::new();
    if let Some(as_idx) = find_top_level_alias_as(source, tokens, start, end_exclusive) {
        push_token_children(b, &mut children, tokens, start, as_idx);
        if let Some(alias_clause) = build_sql_alias_clause(b, tokens, as_idx, end_exclusive) {
            children.push(alias_clause);
        } else {
            push_token_children(b, &mut children, tokens, as_idx, end_exclusive);
        }
    } else {
        push_token_children(b, &mut children, tokens, start, end_exclusive);
    }
    let range = tokens[start].range.start..tokens[end_exclusive - 1].range.end;
    Some(b.branch(SyntaxKind::SqlDataSource, range, &children))
}

fn build_sql_predicate_branch(
    b: &mut SyntaxTreeBuilder,
    kind: SyntaxKind,
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> Option<NodeId> {
    if start >= end_exclusive {
        return None;
    }
    let mut children = Vec::new();
    children.push(token_leaf(b, &tokens[start]));
    let predicate_start = skip_trivia(tokens, start + 1);
    if kind == SyntaxKind::SelectWhereClause {
        if predicate_start < end_exclusive
            && tokens.get(predicate_start).map(|token| token.kind) == Some(TokenKind::LParen)
            && let Some(dynamic_end) = find_matching_delim(
                tokens,
                predicate_start,
                TokenKind::LParen,
                TokenKind::RParen,
            )
            && dynamic_end + 1 == end_exclusive
            && let Some(dynamic_node) = build_token_branch(
                b,
                SyntaxKind::SqlDynamicWhere,
                tokens,
                predicate_start,
                dynamic_end + 1,
            )
        {
            children.push(dynamic_node);
        } else if let Some(predicate_node) =
            build_sql_predicate_expr(b, source, tokens, predicate_start, end_exclusive)
        {
            children.push(predicate_node);
        }
    } else if let Some(predicate_node) =
        build_sql_predicate_expr(b, source, tokens, predicate_start, end_exclusive)
    {
        children.push(predicate_node);
    } else {
        push_token_children(b, &mut children, tokens, predicate_start, end_exclusive);
    }
    let range = tokens[start].range.start..tokens[end_exclusive - 1].range.end;
    Some(b.branch(kind, range, &children))
}

fn build_select_join_clause(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> Option<NodeId> {
    if start >= end_exclusive {
        return None;
    }
    let join_kw_idx = find_top_level_keyword(source, tokens, start, end_exclusive, "join")?;
    let source_start = skip_trivia(tokens, join_kw_idx + 1);
    let on_idx = find_top_level_keyword(source, tokens, source_start, end_exclusive, "on")
        .unwrap_or(end_exclusive);
    let mut children = Vec::new();
    push_token_children(b, &mut children, tokens, start, source_start);
    if let Some(source_node) = build_sql_data_source(b, source, tokens, source_start, on_idx) {
        children.push(source_node);
    }
    if on_idx < end_exclusive
        && let Some(on_node) = build_sql_predicate_branch(
            b,
            SyntaxKind::SqlPredicateExpr,
            source,
            tokens,
            on_idx,
            end_exclusive,
        )
    {
        children.push(on_node);
    }
    let range = tokens[start].range.start..tokens[end_exclusive - 1].range.end;
    Some(b.branch(SyntaxKind::SelectJoinClause, range, &children))
}

fn build_select_from_clause(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> Option<NodeId> {
    if start >= end_exclusive {
        return None;
    }
    let mut children = Vec::new();
    children.push(token_leaf(b, &tokens[start]));
    let mut cursor = skip_trivia(tokens, start + 1);
    if cursor < end_exclusive {
        let first_join = scan_until_clause(tokens, cursor, end_exclusive, |tokens, idx| {
            select_join_starts(source, tokens, idx)
        });
        if let Some(source_node) = build_sql_data_source(b, source, tokens, cursor, first_join) {
            children.push(source_node);
        }
        cursor = first_join;
        while cursor < end_exclusive {
            let join_end = scan_until_clause(tokens, cursor + 1, end_exclusive, |tokens, idx| {
                select_join_starts(source, tokens, idx)
            });
            if let Some(join_node) = build_select_join_clause(b, source, tokens, cursor, join_end) {
                children.push(join_node);
            } else {
                push_token_children(b, &mut children, tokens, cursor, join_end);
            }
            cursor = join_end;
        }
    }
    let range = tokens[start].range.start..tokens[end_exclusive - 1].range.end;
    Some(b.branch(SyntaxKind::SelectFromClause, range, &children))
}

fn find_projection_alias_start(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> Option<usize> {
    find_top_level_alias_as(source, tokens, start, end_exclusive)
}

fn build_sql_projection_item(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> Option<NodeId> {
    if start >= end_exclusive {
        return None;
    }
    let mut children = Vec::new();
    let alias_start =
        find_projection_alias_start(source, tokens, start, end_exclusive).unwrap_or(end_exclusive);
    push_sql_expr_children(
        b,
        &mut children,
        source,
        tokens,
        start,
        alias_start,
        SqlExprMode::Structured,
    );
    if alias_start < end_exclusive {
        if let Some(alias_clause) = build_sql_alias_clause(b, tokens, alias_start, end_exclusive) {
            children.push(alias_clause);
        } else {
            push_token_children(b, &mut children, tokens, alias_start, end_exclusive);
        }
    }
    let range = tokens[start].range.start..tokens[end_exclusive - 1].range.end;
    Some(b.branch(SyntaxKind::SqlProjectionItem, range, &children))
}

fn build_select_projection_list(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> Option<NodeId> {
    if start >= end_exclusive {
        return None;
    }
    let mut children = Vec::new();
    let mut item_start = skip_trivia(tokens, start);
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut sql_case_depth = 0i32;
    let mut idx = start;
    while idx < end_exclusive {
        match tokens[idx].kind {
            TokenKind::LParen => paren += 1,
            TokenKind::RParen => paren -= 1,
            TokenKind::LBracket => bracket += 1,
            TokenKind::RBracket => bracket -= 1,
            TokenKind::LBrace => brace += 1,
            TokenKind::RBrace => brace -= 1,
            TokenKind::Ident if is_keyword(source, &tokens[idx], "case") => sql_case_depth += 1,
            TokenKind::Ident if is_keyword(source, &tokens[idx], "end") && sql_case_depth > 0 => {
                sql_case_depth -= 1
            }
            TokenKind::Comma if paren == 0 && bracket == 0 && brace == 0 && sql_case_depth == 0 => {
                let item_end = trim_trailing_comment_tokens(tokens, item_start, idx);
                if let Some(item) =
                    build_sql_projection_item(b, source, tokens, item_start, item_end)
                {
                    children.push(item);
                }
                item_start = skip_trivia(tokens, idx + 1);
            }
            _ => {}
        }
        idx += 1;
    }
    let item_end = trim_trailing_comment_tokens(tokens, item_start, end_exclusive);
    if let Some(item) = build_sql_projection_item(b, source, tokens, item_start, item_end) {
        children.push(item);
    }
    let range = tokens[start].range.start..tokens[end_exclusive - 1].range.end;
    Some(b.branch(SyntaxKind::SelectProjectionList, range, &children))
}

fn build_select_clause(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    kind: SelectClauseKind,
    start: usize,
    end_exclusive: usize,
) -> Option<NodeId> {
    match kind {
        SelectClauseKind::Distinct => build_token_branch(
            b,
            SyntaxKind::SelectDistinctClause,
            tokens,
            start,
            end_exclusive,
        ),
        SelectClauseKind::Fields => {
            let fields_start = skip_trivia(tokens, start + 1);
            build_select_projection_list(b, source, tokens, fields_start, end_exclusive)
        }
        SelectClauseKind::UpTo => build_token_branch(
            b,
            SyntaxKind::SelectUpToClause,
            tokens,
            start,
            end_exclusive,
        ),
        SelectClauseKind::PackageSize => build_token_branch(
            b,
            SyntaxKind::SelectPackageSizeClause,
            tokens,
            start,
            end_exclusive,
        ),
        SelectClauseKind::Offset => build_token_branch(
            b,
            SyntaxKind::SelectOffsetClause,
            tokens,
            start,
            end_exclusive,
        ),
        SelectClauseKind::AbapOptions => build_token_branch(
            b,
            SyntaxKind::SelectAbapOptionsClause,
            tokens,
            start,
            end_exclusive,
        ),
        SelectClauseKind::SetOperator => build_token_branch(
            b,
            SyntaxKind::SelectSetOperatorClause,
            tokens,
            start,
            end_exclusive,
        ),
        SelectClauseKind::From => build_select_from_clause(b, source, tokens, start, end_exclusive),
        SelectClauseKind::Into | SelectClauseKind::Appending => {
            let mut children = Vec::new();
            let target_end = push_select_target_clause_children(
                b,
                &mut children,
                source,
                tokens,
                start,
                end_exclusive,
            );
            push_token_children(b, &mut children, tokens, target_end, end_exclusive);
            let range = tokens[start].range.start..tokens[end_exclusive - 1].range.end;
            Some(b.branch(SyntaxKind::SelectIntoClause, range, &children))
        }
        SelectClauseKind::Where => build_sql_predicate_branch(
            b,
            SyntaxKind::SelectWhereClause,
            source,
            tokens,
            start,
            end_exclusive,
        ),
        SelectClauseKind::GroupBy => build_token_branch(
            b,
            SyntaxKind::SelectGroupByClause,
            tokens,
            start,
            end_exclusive,
        ),
        SelectClauseKind::Having => build_sql_predicate_branch(
            b,
            SyntaxKind::SelectHavingClause,
            source,
            tokens,
            start,
            end_exclusive,
        ),
        SelectClauseKind::OrderBy => build_token_branch(
            b,
            SyntaxKind::SelectOrderByClause,
            tokens,
            start,
            end_exclusive,
        ),
        SelectClauseKind::ForAllEntries => build_token_branch(
            b,
            SyntaxKind::SelectForAllEntriesClause,
            tokens,
            start,
            end_exclusive,
        ),
        SelectClauseKind::ForUpdate => build_token_branch(
            b,
            SyntaxKind::SelectForUpdateClause,
            tokens,
            start,
            end_exclusive,
        ),
    }
}

fn select_set_operator_tail_clause_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(kind) = select_clause_start_kind(source, tokens, idx) else {
        return false;
    };
    matches!(
        kind,
        SelectClauseKind::Into
            | SelectClauseKind::Appending
            | SelectClauseKind::UpTo
            | SelectClauseKind::PackageSize
            | SelectClauseKind::Offset
            | SelectClauseKind::AbapOptions
            | SelectClauseKind::SetOperator
    )
}

fn scan_select_set_operator_clause_end(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> usize {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut sql_case_depth = 0i32;
    let mut idx = start;
    while idx < end_exclusive {
        let token = &tokens[idx];
        if paren == 0
            && bracket == 0
            && brace == 0
            && sql_case_depth == 0
            && select_set_operator_tail_clause_starts(source, tokens, idx)
        {
            return idx;
        }
        match token.kind {
            TokenKind::LParen => paren += 1,
            TokenKind::RParen => paren -= 1,
            TokenKind::LBracket => bracket += 1,
            TokenKind::RBracket => bracket -= 1,
            TokenKind::LBrace => brace += 1,
            TokenKind::RBrace => brace -= 1,
            TokenKind::Ident if is_keyword(source, token, "case") => sql_case_depth += 1,
            TokenKind::Ident if is_keyword(source, token, "end") && sql_case_depth > 0 => {
                sql_case_depth -= 1
            }
            _ => {}
        }
        idx += 1;
    }
    end_exclusive
}

fn previous_non_comment_token(tokens: &[Token], before: usize) -> Option<usize> {
    let mut idx = before.checked_sub(1)?;
    loop {
        if tokens
            .get(idx)
            .is_some_and(|token| token.kind != TokenKind::Comment)
        {
            return Some(idx);
        }
        idx = idx.checked_sub(1)?;
    }
}

fn select_token_is_set_operand_lead(source: &str, tokens: &[Token], select_idx: usize) -> bool {
    let Some(prev_idx) = previous_non_comment_token(tokens, select_idx) else {
        return false;
    };
    let prev = &tokens[prev_idx];
    if select_set_operator_starts(source, tokens, prev_idx) {
        return true;
    }
    if (is_keyword(source, prev, "all") || is_keyword(source, prev, "distinct"))
        && let Some(operator_idx) = previous_non_comment_token(tokens, prev_idx)
    {
        return select_set_operator_starts(source, tokens, operator_idx);
    }
    false
}

fn parse_select_header_until_period(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> (Vec<NodeId>, usize) {
    match scan_select_stmt_period(tokens, source, idx + 1) {
        StmtPeriodScan::Found(period_i) => {
            let mut query_children = Vec::new();
            let mut cursor = idx + 1;
            if tokens
                .get(cursor)
                .is_some_and(|token| is_keyword(source, token, "single"))
            {
                query_children.push(token_leaf(b, &tokens[cursor]));
                cursor += 1;
            }
            while cursor < period_i {
                let next_clause = scan_until_clause(tokens, cursor, period_i, |tokens, idx| {
                    select_clause_start_kind(source, tokens, idx).is_some()
                });
                if next_clause > cursor {
                    if let Some(projection) =
                        build_select_projection_list(b, source, tokens, cursor, next_clause)
                    {
                        query_children.push(projection);
                    }
                    cursor = next_clause;
                    continue;
                }
                let Some(kind) = select_clause_start_kind(source, tokens, cursor) else {
                    break;
                };
                let clause_end = match kind {
                    SelectClauseKind::Distinct => skip_trivia(tokens, cursor + 1),
                    SelectClauseKind::SetOperator => {
                        scan_select_set_operator_clause_end(source, tokens, cursor + 1, period_i)
                    }
                    _ => scan_until_clause(tokens, cursor + 1, period_i, |tokens, idx| {
                        select_clause_start_kind(source, tokens, idx).is_some()
                    }),
                };
                if let Some(clause) =
                    build_select_clause(b, source, tokens, kind, cursor, clause_end)
                {
                    query_children.push(clause);
                }
                cursor = clause_end;
            }

            let mut children = Vec::new();
            if !query_children.is_empty() {
                let query_range =
                    b.span(query_children[0]).start..b.span(*query_children.last().unwrap()).end;
                children.push(b.branch(SyntaxKind::SelectQuery, query_range, &query_children));
            }
            children.push(token_leaf(b, &tokens[period_i]));
            (children, period_i + 1)
        }
        StmtPeriodScan::Unterminated { end_exclusive } => {
            let start_tok = &tokens[idx];
            let err_end = unterminated_err_end(tokens, end_exclusive, start_tok.range.end);
            errors.push(crate::ParseError {
                message: "syntax error: expected '.' after SELECT statement".to_string(),
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
    }
}

fn scan_select_stmt_period(tokens: &[Token], source: &str, start: usize) -> StmtPeriodScan {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut sql_case_depth = 0i32;
    let mut allow_line_start_named_args = false;
    let mut allow_line_start_condition_comparison = false;
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
            if is_named_arg_clause_keyword(source, t) {
                allow_line_start_named_args = true;
            }
            if is_condition_continuation_keyword(source, t) {
                allow_line_start_condition_comparison = true;
            }
            let is_sql_case_start = t.kind == TokenKind::Ident && is_keyword(source, t, "case");
            let is_sql_case_branch = t.kind == TokenKind::Ident
                && (is_keyword(source, t, "when") || is_keyword(source, t, "else"));
            let is_sql_case_end = t.kind == TokenKind::Ident && is_keyword(source, t, "end");
            if i > start {
                let condition_continuation = allow_line_start_condition_comparison
                    && line_start_condition_operand_continues(source, tokens, i);
                let named_arg_continuation =
                    allow_line_start_named_args && line_start_named_arg_continues(tokens, i);
                if t.kind == TokenKind::Ident
                    && token_begins_line(source, t)
                    && is_definite_stmt_lead_keyword(source, t)
                    && !(is_keyword(source, t, "select")
                        && select_token_is_set_operand_lead(source, tokens, i))
                    && !(is_sql_case_start
                        || is_sql_case_branch && sql_case_depth > 0
                        || is_sql_case_end && sql_case_depth > 0)
                    && !named_arg_continuation
                    && !condition_continuation
                {
                    return StmtPeriodScan::Unterminated { end_exclusive: i };
                }
                if t.kind == TokenKind::Ident && token_begins_line(source, t) {
                    let next_kind = tokens.get(i + 1).map(|x| x.kind);
                    if !allow_line_start_named_args
                        && !allow_line_start_condition_comparison
                        && matches!(next_kind, Some(TokenKind::Eq | TokenKind::QuestionEq))
                    {
                        return StmtPeriodScan::Unterminated { end_exclusive: i };
                    }
                }
            }
            if is_sql_case_start {
                sql_case_depth += 1;
            } else if is_sql_case_end && sql_case_depth > 0 {
                sql_case_depth -= 1;
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
                if starts_with_table_key_clause(source, tokens, i) {
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
                let table_key_continuation =
                    inside_key_components && line_start_table_key_component_continues(tokens, i);
                if is_definite_stmt_lead_keyword(source, t) {
                    if !is_inline_decl_continuation(source, tokens, i) && !table_key_continuation {
                        return StmtPeriodScan::Unterminated { end_exclusive: i };
                    }
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

fn scan_update_stmt_period(tokens: &[Token], source: &str, start: usize) -> StmtPeriodScan {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut inside_set_clause = false;
    let mut allow_line_start_condition_comparison = false;
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
                if is_keyword(source, t, "set") {
                    inside_set_clause = true;
                } else if is_keyword(source, t, "where")
                    || is_keyword(source, t, "from")
                    || is_keyword(source, t, "using")
                    || is_keyword(source, t, "connection")
                    || is_keyword(source, t, "client")
                {
                    inside_set_clause = false;
                }
            }
            if is_condition_continuation_keyword(source, t) {
                allow_line_start_condition_comparison = true;
            }

            if i > start && t.kind == TokenKind::Ident && token_begins_line(source, t) {
                let condition_continuation = allow_line_start_condition_comparison
                    && line_start_condition_operand_continues(source, tokens, i);
                if is_definite_stmt_lead_keyword(source, t)
                    && !is_inline_decl_continuation(source, tokens, i)
                    && !condition_continuation
                {
                    return StmtPeriodScan::Unterminated { end_exclusive: i };
                }
                if !inside_set_clause {
                    let next_kind = tokens.get(i + 1).map(|x| x.kind);
                    if !allow_line_start_condition_comparison
                        && matches!(next_kind, Some(TokenKind::Eq | TokenKind::QuestionEq))
                    {
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

fn match_keyword_sequence(
    source: &str,
    tokens: &[Token],
    idx: usize,
    parts: &[&str],
) -> Option<usize> {
    let mut i = idx;
    for part in parts {
        let tok = tokens.get(i)?;
        if !is_keyword(source, tok, part) {
            return None;
        }
        i += 1;
    }
    Some(i)
}

fn call_like_lead_kind(
    source: &str,
    tokens: &[Token],
    idx: usize,
) -> Option<(CallLikeLeadKind, usize)> {
    for (parts, kind) in CALL_LIKE_LEADS {
        if let Some(next) = match_keyword_sequence(source, tokens, idx, parts) {
            return Some((*kind, next));
        }
    }
    if is_keyword(source, tokens.get(idx)?, "call")
        && tokens.get(idx + 1).is_some_and(|next| {
            next.kind != TokenKind::Period
                && !NON_SYSTEM_CALL_VARIANTS
                    .iter()
                    .any(|variant| is_keyword(source, next, variant))
        })
    {
        return Some((CallLikeLeadKind::SystemFunctionCall, idx + 1));
    }
    None
}

fn token_children(
    b: &mut SyntaxTreeBuilder,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> Vec<NodeId> {
    error_token_children(b, tokens, start, end_exclusive)
}

fn parse_stmt_with_period_scan<F>(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    scan_start: usize,
    start_tok: &Token,
    missing_period_message: &str,
    errors: &mut Vec<crate::ParseError>,
    next_on_unterminated: fn(&[Token], usize) -> usize,
    on_found: F,
) -> (NodeId, usize)
where
    F: FnOnce(&mut SyntaxTreeBuilder, usize, &mut Vec<crate::ParseError>) -> (NodeId, usize),
{
    match scan_until_statement_period(tokens, source, scan_start) {
        StmtPeriodScan::Found(period_i) => on_found(b, period_i, errors),
        StmtPeriodScan::Unterminated { end_exclusive } => {
            let err_end = unterminated_err_end(tokens, end_exclusive, start_tok.range.end);
            errors.push(crate::ParseError {
                message: missing_period_message.to_string(),
                range: start_tok.range.start..err_end,
            });
            let children = token_children(b, tokens, idx, end_exclusive);
            let node = b.branch(SyntaxKind::Error, start_tok.range.start..err_end, &children);
            (node, next_on_unterminated(tokens, end_exclusive))
        }
    }
}

fn scan_until_chained_statement_period(
    tokens: &[Token],
    source: &str,
    start: usize,
) -> StmtPeriodScan {
    let chain_start = skip_trivia(tokens, start);
    if tokens
        .get(chain_start)
        .is_some_and(|token| token.kind == TokenKind::Colon)
        && let Some(period_i) = scan_until_top_level_period(tokens, chain_start + 1)
    {
        return StmtPeriodScan::Found(period_i);
    }
    scan_until_statement_period(tokens, source, start)
}

fn parse_chained_stmt_with_period_scan<F>(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    scan_start: usize,
    start_tok: &Token,
    missing_period_message: &str,
    errors: &mut Vec<crate::ParseError>,
    next_on_unterminated: fn(&[Token], usize) -> usize,
    on_found: F,
) -> (NodeId, usize)
where
    F: FnOnce(&mut SyntaxTreeBuilder, usize, &mut Vec<crate::ParseError>) -> (NodeId, usize),
{
    match scan_until_chained_statement_period(tokens, source, scan_start) {
        StmtPeriodScan::Found(period_i) => on_found(b, period_i, errors),
        StmtPeriodScan::Unterminated { end_exclusive } => {
            let err_end = unterminated_err_end(tokens, end_exclusive, start_tok.range.end);
            errors.push(crate::ParseError {
                message: missing_period_message.to_string(),
                range: start_tok.range.start..err_end,
            });
            let children = token_children(b, tokens, idx, end_exclusive);
            let node = b.branch(SyntaxKind::Error, start_tok.range.start..err_end, &children);
            (node, next_on_unterminated(tokens, end_exclusive))
        }
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
    Some(parse_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        idx + 1,
        tok,
        missing_period_message,
        errors,
        next_after_unterminated_scan,
        |b, period_i, _errors| {
            let children = token_children(b, tokens, idx, period_i + 1);
            let node = b.branch(kind, tok.range.start..tokens[period_i].range.end, &children);
            (node, period_i + 1)
        },
    ))
}

fn build_include_stmt_children(
    b: &mut SyntaxTreeBuilder,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let mut children = Vec::with_capacity(period_i + 1 - idx);
    let mut expect_name = false;
    for i in idx..=period_i {
        let token = &tokens[i];
        if i == idx {
            children.push(token_leaf(b, token));
            expect_name = true;
            continue;
        }
        if token.kind == TokenKind::Comment {
            children.push(token_leaf(b, token));
            continue;
        }
        if expect_name && token.kind == TokenKind::Ident {
            let leaf = token_leaf(b, token);
            children.push(b.branch(SyntaxKind::IncludeName, token.range.clone(), &[leaf]));
            expect_name = false;
            continue;
        }
        if matches!(token.kind, TokenKind::Colon | TokenKind::Comma) {
            expect_name = true;
        } else if token.kind != TokenKind::Period {
            expect_name = false;
        }
        children.push(token_leaf(b, token));
    }
    children
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

fn try_parse_data_inline_decl(
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
    let node = b.branch(
        SyntaxKind::DataInlineDecl,
        data_tok.range.start..rparen.range.end,
        &[data_leaf, lparen_leaf, name, rparen_leaf],
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

fn push_wrapped_expr_child(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
    prev_before_first: Option<&Token>,
    wrapper_kind: SyntaxKind,
) {
    if start >= end_exclusive {
        return;
    }
    let expr = parse_arithmetic_expr(b, source, &tokens[start..end_exclusive], prev_before_first);
    children.push(b.branch(
        wrapper_kind,
        tokens[start].range.start..tokens[end_exclusive - 1].range.end,
        &[expr],
    ));
}

fn push_wrapped_data_inline_decl_child(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    source: &str,
    tokens: &[Token],
    start: usize,
    wrapper_kind: SyntaxKind,
) -> Option<usize> {
    let (decl, next_i) = try_parse_data_inline_decl(b, source, tokens, start)?;
    children.push(b.branch(
        wrapper_kind,
        tokens[start].range.start..tokens[next_i - 1].range.end,
        &[decl],
    ));
    Some(next_i)
}

fn push_logical_expr_child(
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
    children.push(parse_logical_expr(
        b,
        source,
        &tokens[start..end_exclusive],
        prev_before_first,
    ));
}

fn push_write_separator_or_position_tokens(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    tokens: &[Token],
    idx: usize,
    end_exclusive: usize,
) -> Option<usize> {
    let token = tokens.get(idx)?;
    if matches!(token.kind, TokenKind::Colon | TokenKind::Comma) {
        children.push(token_leaf(b, token));
        return Some(idx + 1);
    }

    if token.kind != TokenKind::Slash {
        return None;
    }

    children.push(token_leaf(b, token));
    let mut next = idx + 1;
    if next < end_exclusive
        && tokens[next].kind == TokenKind::Number
        && !have_space_between(token, &tokens[next])
    {
        children.push(token_leaf(b, &tokens[next]));
        next += 1;
        if next < end_exclusive
            && tokens[next].kind == TokenKind::LParen
            && !have_space_between(&tokens[next - 1], &tokens[next])
        {
            let mut depth = 0i32;
            while next < end_exclusive {
                let current = &tokens[next];
                children.push(token_leaf(b, current));
                match current.kind {
                    TokenKind::LParen => depth += 1,
                    TokenKind::RParen => {
                        depth -= 1;
                        next += 1;
                        if depth == 0 {
                            break;
                        }
                        continue;
                    }
                    _ => {}
                }
                next += 1;
            }
        }
    }

    Some(next)
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

fn find_matching_delim(
    tokens: &[Token],
    start: usize,
    open: TokenKind,
    close: TokenKind,
) -> Option<usize> {
    let mut depth = 0i32;
    let mut idx = start;
    while idx < tokens.len() {
        let token = &tokens[idx];
        if token.kind == open {
            depth += 1;
        } else if token.kind == close {
            depth -= 1;
            if depth == 0 {
                return Some(idx);
            }
        }
        idx += 1;
    }
    None
}

fn match_submit_sequence(
    source: &str,
    tokens: &[Token],
    idx: usize,
    parts: &[&str],
) -> Option<usize> {
    let mut i = idx;
    for part in parts {
        let tok = tokens.get(i)?;
        if *part == "-" {
            if tok.kind != TokenKind::Minus {
                return None;
            }
        } else if !is_keyword(source, tok, part) {
            return None;
        }
        i += 1;
    }
    Some(i)
}

fn submit_clause_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    match_submit_sequence(source, tokens, idx, &["using", "selection", "-", "screen"]).is_some()
        || match_submit_sequence(source, tokens, idx, &["via", "selection", "-", "screen"])
            .is_some()
        || match_submit_sequence(source, tokens, idx, &["using", "selection", "-", "set"]).is_some()
        || match_submit_sequence(
            source,
            tokens,
            idx,
            &["using", "selection", "-", "sets", "of", "program"],
        )
        .is_some()
        || match_submit_sequence(source, tokens, idx, &["with", "selection", "-", "table"])
            .is_some()
        || match_submit_sequence(source, tokens, idx, &["with", "free", "selections"]).is_some()
        || match_submit_sequence(source, tokens, idx, &["with"]).is_some()
        || match_submit_sequence(source, tokens, idx, &["line", "-", "size"]).is_some()
        || match_submit_sequence(source, tokens, idx, &["line", "-", "count"]).is_some()
        || match_submit_sequence(source, tokens, idx, &["exporting", "list", "to", "memory"])
            .is_some()
        || match_submit_sequence(source, tokens, idx, &["to", "sap", "-", "spool"]).is_some()
        || match_submit_sequence(source, tokens, idx, &["spool", "parameters"]).is_some()
        || match_submit_sequence(source, tokens, idx, &["archive", "parameters"]).is_some()
        || match_submit_sequence(source, tokens, idx, &["without", "spool", "dynpro"]).is_some()
        || match_submit_sequence(source, tokens, idx, &["user"]).is_some()
        || match_submit_sequence(source, tokens, idx, &["via", "job"]).is_some()
        || match_submit_sequence(source, tokens, idx, &["number"]).is_some()
        || match_submit_sequence(source, tokens, idx, &["language"]).is_some()
        || match_submit_sequence(source, tokens, idx, &["and", "return"]).is_some()
}

fn submit_is_comparison_operator(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    token.kind == TokenKind::Eq
        || (token.kind == TokenKind::Ident
            && matches!(
                token.lexeme(source).to_ascii_uppercase().as_str(),
                "EQ" | "NE" | "CP" | "NP" | "GE" | "GT" | "LE" | "LT"
            ))
}

fn scan_submit_expr_end(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
    stop_clause: impl Fn(&[Token], usize) -> bool,
) -> usize {
    scan_until_clause(tokens, start, end_exclusive, |tokens, idx| {
        stop_clause(tokens, idx) || submit_clause_starts(source, tokens, idx)
    })
}

fn scan_and_push_expr_clause<F>(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    source: &str,
    tokens: &[Token],
    expr_start: usize,
    end_exclusive: usize,
    prev_before_first: Option<&Token>,
    clause_starts: &F,
) -> usize
where
    F: Fn(&[Token], usize) -> bool,
{
    let expr_end = scan_until_clause(tokens, expr_start, end_exclusive, clause_starts);
    push_expr_child(
        b,
        children,
        source,
        tokens,
        expr_start,
        expr_end,
        prev_before_first,
    );
    expr_end
}

fn scan_and_push_sql_host_or_expr_clause<F>(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    source: &str,
    tokens: &[Token],
    expr_start: usize,
    end_exclusive: usize,
    prev_before_first: Option<&Token>,
    clause_starts: &F,
) -> usize
where
    F: Fn(&[Token], usize) -> bool,
{
    let expr_end = scan_until_clause(tokens, expr_start, end_exclusive, clause_starts);
    if let Some((host_expr, next_idx)) =
        build_sql_host_expr(b, source, tokens, expr_start, expr_end)
        && next_idx == expr_end
    {
        children.push(host_expr);
    } else {
        push_expr_child(
            b,
            children,
            source,
            tokens,
            expr_start,
            expr_end,
            prev_before_first,
        );
    }
    expr_end
}

fn scan_and_push_logical_expr_clause<F>(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    source: &str,
    tokens: &[Token],
    expr_start: usize,
    end_exclusive: usize,
    prev_before_first: Option<&Token>,
    clause_starts: &F,
) -> usize
where
    F: Fn(&[Token], usize) -> bool,
{
    let expr_end = scan_until_clause(tokens, expr_start, end_exclusive, clause_starts);
    push_logical_expr_child(
        b,
        children,
        source,
        tokens,
        expr_start,
        expr_end,
        prev_before_first,
    );
    expr_end
}

fn scan_and_push_assigning_target_clause<F>(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    source: &str,
    tokens: &[Token],
    expr_start: usize,
    end_exclusive: usize,
    prev_before_first: &Token,
    clause_starts: &F,
) -> usize
where
    F: Fn(&[Token], usize) -> bool,
{
    if let Some((inline_decl, next_i)) =
        try_parse_field_symbol_inline_decl(b, source, tokens, expr_start)
    {
        children.push(inline_decl);
        next_i
    } else {
        scan_and_push_expr_clause(
            b,
            children,
            source,
            tokens,
            expr_start,
            end_exclusive,
            Some(prev_before_first),
            clause_starts,
        )
    }
}

fn scan_and_push_reference_into_clause<F>(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    source: &str,
    tokens: &[Token],
    into_idx: usize,
    end_exclusive: usize,
    clause_starts: &F,
) -> usize
where
    F: Fn(&[Token], usize) -> bool,
{
    children.push(token_leaf(b, &tokens[into_idx - 1]));
    children.push(token_leaf(b, &tokens[into_idx]));
    scan_and_push_expr_clause(
        b,
        children,
        source,
        tokens,
        into_idx + 1,
        end_exclusive,
        Some(&tokens[into_idx]),
        clause_starts,
    )
}

fn named_argument_section_keyword(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    if tokens.get(idx + 1).map(|next| next.kind) == Some(TokenKind::Eq) {
        return false;
    }
    is_keyword(source, token, "exporting")
        || is_keyword(source, token, "importing")
        || is_keyword(source, token, "changing")
        || is_keyword(source, token, "tables")
        || is_keyword(source, token, "receiving")
        || is_keyword(source, token, "exceptions")
        || is_keyword(source, token, "source")
        || is_keyword(source, token, "result")
        || is_keyword(source, token, "xml")
}

fn call_argument_value_end(
    source: &str,
    tokens: &[Token],
    start_idx: usize,
    end_exclusive: usize,
) -> usize {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut idx = start_idx;
    while idx < end_exclusive {
        let token = &tokens[idx];
        match token.kind {
            TokenKind::LParen => paren += 1,
            TokenKind::RParen => paren -= 1,
            TokenKind::LBracket => bracket += 1,
            TokenKind::RBracket => bracket -= 1,
            TokenKind::LBrace => brace += 1,
            TokenKind::RBrace => brace -= 1,
            _ => {}
        }
        if paren == 0 && bracket == 0 && brace == 0 {
            if named_argument_section_keyword(source, tokens, idx) {
                break;
            }
            if token.kind == TokenKind::Ident
                && tokens.get(idx + 1).map(|next| next.kind) == Some(TokenKind::Eq)
            {
                break;
            }
        }
        idx += 1;
    }
    idx
}

fn push_call_argument_value_child(
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
    let inline_end = trim_trailing_comment_tokens(tokens, start, end_exclusive);
    if let Some((inline_decl, next_idx)) = try_parse_data_inline_decl(b, source, tokens, start)
        && next_idx == inline_end
    {
        children.push(inline_decl);
        return;
    }
    if let Some((inline_decl, next_idx)) =
        try_parse_field_symbol_inline_decl(b, source, tokens, start)
        && next_idx == inline_end
    {
        children.push(inline_decl);
        return;
    }
    push_expr_child(
        b,
        children,
        source,
        tokens,
        start,
        end_exclusive,
        prev_before_first,
    );
}

fn push_call_positional_arg_node(
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
    let mut positional_children = Vec::new();
    push_call_argument_value_child(
        b,
        &mut positional_children,
        source,
        tokens,
        start,
        end_exclusive,
        prev_before_first,
    );
    if !positional_children.is_empty() {
        children.push(b.branch(
            SyntaxKind::CallPositionalArg,
            tokens[start].range.start..tokens[end_exclusive - 1].range.end,
            &positional_children,
        ));
    }
}

fn trim_trailing_comment_tokens(tokens: &[Token], start: usize, end_exclusive: usize) -> usize {
    let mut end = end_exclusive;
    while end > start && tokens[end - 1].kind == TokenKind::Comment {
        end -= 1;
    }
    end
}

fn build_call_argument_list_node(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> Option<NodeId> {
    if start >= end_exclusive {
        return None;
    }
    let mut children = Vec::new();
    let mut idx = start;
    let mut segment_start = start;

    while idx < end_exclusive {
        let token = &tokens[idx];
        if token.kind == TokenKind::Comment {
            idx += 1;
            continue;
        }
        if named_argument_section_keyword(source, tokens, idx) {
            if segment_start < idx {
                let prev = segment_start
                    .checked_sub(1)
                    .and_then(|prev_idx| tokens.get(prev_idx));
                let mut positional_children = Vec::new();
                push_call_argument_value_child(
                    b,
                    &mut positional_children,
                    source,
                    tokens,
                    segment_start,
                    idx,
                    prev,
                );
                if !positional_children.is_empty() {
                    children.push(b.branch(
                        SyntaxKind::CallPositionalArg,
                        tokens[segment_start].range.start..tokens[idx - 1].range.end,
                        &positional_children,
                    ));
                }
            }
            let section_leaf = token_leaf(b, token);
            children.push(b.branch(
                SyntaxKind::CallArgSection,
                token.range.clone(),
                &[section_leaf],
            ));
            idx += 1;
            segment_start = idx;
            continue;
        }
        if token.kind == TokenKind::Ident
            && tokens.get(idx + 1).map(|next| next.kind) == Some(TokenKind::Eq)
        {
            if segment_start < idx {
                let prev = segment_start
                    .checked_sub(1)
                    .and_then(|prev_idx| tokens.get(prev_idx));
                let mut positional_children = Vec::new();
                push_call_argument_value_child(
                    b,
                    &mut positional_children,
                    source,
                    tokens,
                    segment_start,
                    idx,
                    prev,
                );
                if !positional_children.is_empty() {
                    children.push(b.branch(
                        SyntaxKind::CallPositionalArg,
                        tokens[segment_start].range.start..tokens[idx - 1].range.end,
                        &positional_children,
                    ));
                }
            }
            let value_end = call_argument_value_end(source, tokens, idx + 2, end_exclusive);
            let mut arg_children = vec![token_leaf(b, token), token_leaf(b, &tokens[idx + 1])];
            push_call_argument_value_child(
                b,
                &mut arg_children,
                source,
                tokens,
                idx + 2,
                value_end,
                Some(&tokens[idx + 1]),
            );
            let arg_end = if value_end > idx + 2 {
                tokens[value_end - 1].range.end
            } else {
                tokens[idx + 1].range.end
            };
            children.push(b.branch(
                SyntaxKind::CallNamedArg,
                token.range.start..arg_end,
                &arg_children,
            ));
            idx = value_end;
            segment_start = idx;
            continue;
        }
        idx += 1;
    }

    if segment_start < end_exclusive {
        let prev = segment_start
            .checked_sub(1)
            .and_then(|prev_idx| tokens.get(prev_idx));
        let mut positional_children = Vec::new();
        push_call_argument_value_child(
            b,
            &mut positional_children,
            source,
            tokens,
            segment_start,
            end_exclusive,
            prev,
        );
        if !positional_children.is_empty() {
            children.push(b.branch(
                SyntaxKind::CallPositionalArg,
                tokens[segment_start].range.start..tokens[end_exclusive - 1].range.end,
                &positional_children,
            ));
        }
    }

    if children.is_empty() {
        return None;
    }
    Some(b.branch(
        SyntaxKind::CallArgList,
        tokens[start].range.start..tokens[end_exclusive - 1].range.end,
        &children,
    ))
}

fn system_call_addition_keyword(source: &str, tokens: &[Token], idx: usize, keyword: &str) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    token.kind != TokenKind::Comment && is_keyword(source, token, keyword)
}

fn system_call_id_clause_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    system_call_addition_keyword(source, tokens, idx, "id")
}

fn system_call_field_clause_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    system_call_addition_keyword(source, tokens, idx, "field")
}

fn scan_until_system_call_id_clause(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> usize {
    scan_until_clause(tokens, start, end_exclusive, |tokens, at| {
        system_call_id_clause_starts(source, tokens, at)
    })
}

fn scan_until_system_call_field_clause(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> usize {
    scan_until_clause(tokens, start, end_exclusive, |tokens, at| {
        system_call_field_clause_starts(source, tokens, at)
    })
}

fn skip_comment_tokens(tokens: &[Token], mut idx: usize, end_exclusive: usize) -> usize {
    while idx < end_exclusive && tokens[idx].kind == TokenKind::Comment {
        idx += 1;
    }
    idx
}

fn push_system_call_arg_section(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    token: &Token,
) {
    let section_leaf = token_leaf(b, token);
    children.push(b.branch(
        SyntaxKind::CallArgSection,
        token.range.clone(),
        &[section_leaf],
    ));
}

fn build_system_function_call_argument_list_node(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> Option<NodeId> {
    if start >= end_exclusive {
        return None;
    }

    let mut children = Vec::new();
    let mut idx = start;
    while idx < end_exclusive {
        idx = skip_comment_tokens(tokens, idx, end_exclusive);
        if idx >= end_exclusive {
            break;
        }

        if !system_call_id_clause_starts(source, tokens, idx) {
            push_call_positional_arg_node(b, &mut children, source, tokens, idx, idx + 1, None);
            idx += 1;
            continue;
        }

        push_system_call_arg_section(b, &mut children, &tokens[idx]);
        idx += 1;

        let id_start = skip_comment_tokens(tokens, idx, end_exclusive);
        let id_end = scan_until_system_call_field_clause(
            source,
            tokens,
            id_start.saturating_add(1).min(end_exclusive),
            end_exclusive,
        );
        push_call_positional_arg_node(
            b,
            &mut children,
            source,
            tokens,
            id_start,
            id_end,
            Some(&tokens[idx - 1]),
        );
        idx = skip_comment_tokens(tokens, id_end, end_exclusive);

        if idx >= end_exclusive || !system_call_field_clause_starts(source, tokens, idx) {
            continue;
        }

        push_system_call_arg_section(b, &mut children, &tokens[idx]);
        idx += 1;

        let field_start = skip_comment_tokens(tokens, idx, end_exclusive);
        let field_end = scan_until_system_call_id_clause(
            source,
            tokens,
            field_start.saturating_add(1).min(end_exclusive),
            end_exclusive,
        );
        push_call_positional_arg_node(
            b,
            &mut children,
            source,
            tokens,
            field_start,
            field_end,
            Some(&tokens[idx - 1]),
        );
        idx = field_end;
    }

    if children.is_empty() {
        return None;
    }
    Some(b.branch(
        SyntaxKind::CallArgList,
        tokens[start].range.start..tokens[end_exclusive - 1].range.end,
        &children,
    ))
}

fn find_top_level_keyword_index(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
    keyword: &str,
) -> Option<usize> {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut idx = start;
    while idx < end_exclusive {
        let token = &tokens[idx];
        if paren == 0 && bracket == 0 && brace == 0 && is_keyword(source, token, keyword) {
            return Some(idx);
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
    None
}

fn match_keyword_sequence_at(
    source: &str,
    tokens: &[Token],
    idx: usize,
    end_exclusive: usize,
    keywords: &[&str],
) -> Option<usize> {
    let mut cursor = idx;
    for keyword in keywords {
        if cursor >= end_exclusive || !is_keyword(source, &tokens[cursor], keyword) {
            return None;
        }
        cursor += 1;
    }
    Some(cursor)
}

fn find_top_level_keyword_sequence_index(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
    keywords: &[&str],
) -> Option<(usize, usize)> {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut idx = start;
    while idx < end_exclusive {
        let token = &tokens[idx];
        if paren == 0
            && bracket == 0
            && brace == 0
            && let Some(sequence_end) =
                match_keyword_sequence_at(source, tokens, idx, end_exclusive, keywords)
        {
            return Some((idx, sequence_end));
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
    None
}

fn find_top_level_token_kind(
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
    kind: TokenKind,
) -> Option<usize> {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut idx = start;
    while idx < end_exclusive {
        let token = &tokens[idx];
        if paren == 0 && bracket == 0 && brace == 0 && token.kind == kind {
            return Some(idx);
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
    None
}

fn find_top_level_hyphenated_keyword_index(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
    parts: &[&str],
) -> Option<usize> {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut idx = start;
    while idx < end_exclusive {
        if paren == 0 && bracket == 0 && brace == 0 {
            if match_hyphenated_keyword(source, tokens, idx, parts).is_some() {
                return Some(idx);
            }
        }
        match tokens[idx].kind {
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
    None
}

fn token_starts_concatenate_operand(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    if !matches!(
        token.kind,
        TokenKind::Ident
            | TokenKind::Number
            | TokenKind::String
            | TokenKind::StringTemplate
            | TokenKind::LParen
            | TokenKind::LBracket
            | TokenKind::LBrace
            | TokenKind::At
            | TokenKind::Hash
    ) {
        return false;
    }
    if token.kind == TokenKind::Ident
        && (is_keyword(source, token, "into")
            || is_keyword(source, token, "separated")
            || is_keyword(source, token, "respecting")
            || is_keyword(source, token, "in"))
    {
        return false;
    }
    let Some(prev) = idx.checked_sub(1).and_then(|prev_idx| tokens.get(prev_idx)) else {
        return true;
    };
    !(prev.kind == TokenKind::Ident
        && (is_keyword(source, prev, "new")
            || is_keyword(source, prev, "ref")
            || is_keyword(source, prev, "to")))
        && have_space_between(prev, token)
        && !matches!(
            prev.kind,
            TokenKind::Arrow
                | TokenKind::FatArrow
                | TokenKind::Tilde
                | TokenKind::Eq
                | TokenKind::Minus
                | TokenKind::Plus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::Lt
                | TokenKind::Gt
                | TokenKind::Le
                | TokenKind::Ge
                | TokenKind::Ne
                | TokenKind::QuestionEq
                | TokenKind::LParen
                | TokenKind::LBracket
                | TokenKind::LBrace
                | TokenKind::At
                | TokenKind::Hash
                | TokenKind::Ampersand
                | TokenKind::Pipe
        )
}

fn consume_concatenate_operand(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
    clause_keywords: &[&str],
) -> usize {
    let mut idx = start;
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut consumed_any = false;

    while idx < end_exclusive {
        let token = &tokens[idx];
        if paren == 0 && bracket == 0 && brace == 0 {
            if token.kind == TokenKind::Period {
                break;
            }
            if token.kind == TokenKind::Ident
                && clause_keywords
                    .iter()
                    .any(|keyword| is_keyword(source, token, keyword))
            {
                break;
            }
            if consumed_any && token_starts_concatenate_operand(source, tokens, idx) {
                break;
            }
        }

        consumed_any = true;
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

fn push_concatenate_entry_children(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    source: &str,
    tokens: &[Token],
    entry_start: usize,
    entry_end: usize,
) -> bool {
    let Some(into_idx) =
        find_top_level_keyword_index(source, tokens, entry_start, entry_end, "into")
    else {
        return false;
    };

    let mut i = entry_start;
    while i < into_idx {
        let end_idx = consume_concatenate_operand(source, tokens, i, into_idx, &["into"]);
        if end_idx == i {
            i += 1;
            continue;
        }
        push_wrapped_expr_child(
            b,
            children,
            source,
            tokens,
            i,
            end_idx,
            tokens
                .get(i.checked_sub(1).unwrap_or(entry_start))
                .filter(|_| i > entry_start),
            SyntaxKind::ConcatenateSourceOperand,
        );
        i = end_idx;
    }

    children.push(token_leaf(b, &tokens[into_idx]));
    if let Some(next_i) = push_wrapped_data_inline_decl_child(
        b,
        children,
        source,
        tokens,
        into_idx + 1,
        SyntaxKind::ConcatenateTargetOperand,
    ) {
        i = next_i;
    } else {
        let target_end = consume_concatenate_operand(
            source,
            tokens,
            into_idx + 1,
            entry_end,
            &["separated", "respecting", "in"],
        );
        if target_end <= into_idx + 1 {
            return false;
        }
        push_wrapped_expr_child(
            b,
            children,
            source,
            tokens,
            into_idx + 1,
            target_end,
            Some(&tokens[into_idx]),
            SyntaxKind::ConcatenateTargetOperand,
        );
        i = target_end;
    }

    while i < entry_end {
        let token = &tokens[i];
        if is_keyword(source, token, "separated")
            && tokens
                .get(i + 1)
                .is_some_and(|next| is_keyword(source, next, "by"))
        {
            children.push(token_leaf(b, token));
            children.push(token_leaf(b, &tokens[i + 1]));
            let sep_start = i + 2;
            let sep_end = consume_concatenate_operand(
                source,
                tokens,
                sep_start,
                entry_end,
                &["respecting", "in"],
            );
            if sep_end <= sep_start {
                return false;
            }
            push_wrapped_expr_child(
                b,
                children,
                source,
                tokens,
                sep_start,
                sep_end,
                Some(&tokens[i + 1]),
                SyntaxKind::ConcatenateSeparatorOperand,
            );
            i = sep_end;
            continue;
        }
        children.push(token_leaf(b, token));
        i += 1;
    }

    true
}

fn push_split_entry_children(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    source: &str,
    tokens: &[Token],
    entry_start: usize,
    entry_end: usize,
) -> bool {
    let Some(at_idx) = find_top_level_keyword_index(source, tokens, entry_start, entry_end, "at")
    else {
        return false;
    };
    let Some(into_idx) =
        find_top_level_keyword_index(source, tokens, at_idx + 1, entry_end, "into")
    else {
        return false;
    };

    push_wrapped_expr_child(
        b,
        children,
        source,
        tokens,
        entry_start,
        at_idx,
        tokens.get(entry_start.saturating_sub(1)),
        SyntaxKind::SplitSourceOperand,
    );
    children.push(token_leaf(b, &tokens[at_idx]));

    let separator_end =
        consume_concatenate_operand(source, tokens, at_idx + 1, into_idx, &["into"]);
    push_wrapped_expr_child(
        b,
        children,
        source,
        tokens,
        at_idx + 1,
        separator_end,
        Some(&tokens[at_idx]),
        SyntaxKind::SplitSeparatorOperand,
    );

    children.push(token_leaf(b, &tokens[into_idx]));
    let mut i = separator_end.max(into_idx + 1);
    if i < entry_end && is_keyword(source, &tokens[i], "table") {
        children.push(token_leaf(b, &tokens[i]));
        i += 1;
    }
    while i < entry_end {
        let token = &tokens[i];
        if is_keyword(source, token, "in") {
            push_token_children(b, children, tokens, i, entry_end);
            break;
        }
        if let Some(next_i) = push_wrapped_data_inline_decl_child(
            b,
            children,
            source,
            tokens,
            i,
            SyntaxKind::SplitTargetOperand,
        ) {
            i = next_i;
            continue;
        }
        let end_idx = consume_concatenate_operand(source, tokens, i, entry_end, &["in"]);
        if end_idx == i {
            children.push(token_leaf(b, token));
            i += 1;
            continue;
        }
        push_wrapped_expr_child(
            b,
            children,
            source,
            tokens,
            i,
            end_idx,
            tokens.get(i.saturating_sub(1)),
            SyntaxKind::SplitTargetOperand,
        );
        i = end_idx;
    }

    true
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

fn push_read_table_entry_children(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    source: &str,
    tokens: &[Token],
    entry_start: usize,
    entry_end: usize,
) -> bool {
    let clause_starts =
        |tokens: &[Token], idx: usize| read_table_clause_starts(source, tokens, idx);
    let source_end = scan_and_push_expr_clause(
        b,
        children,
        source,
        tokens,
        entry_start,
        entry_end,
        entry_start.checked_sub(1).and_then(|idx| tokens.get(idx)),
        &clause_starts,
    );
    if source_end == entry_start {
        return false;
    }

    let mut i = source_end;
    while i < entry_end {
        let token = &tokens[i];
        if is_keyword(source, token, "into") {
            children.push(token_leaf(b, token));
            let target_start = skip_trivia(tokens, i + 1);
            let target_end = scan_until_clause(tokens, target_start, entry_end, &clause_starts);
            if let Some((inline_decl, next_idx)) =
                try_parse_data_inline_decl(b, source, tokens, target_start)
                && skip_trivia(tokens, next_idx) == target_end
            {
                children.push(inline_decl);
                i = target_end;
            } else {
                push_expr_child(
                    b,
                    children,
                    source,
                    tokens,
                    target_start,
                    target_end,
                    Some(token),
                );
                i = target_end;
            }
            continue;
        }
        if is_keyword(source, token, "assigning") {
            children.push(token_leaf(b, token));
            i = scan_and_push_assigning_target_clause(
                b,
                children,
                source,
                tokens,
                i + 1,
                entry_end,
                token,
                &clause_starts,
            );
            continue;
        }
        if is_keyword(source, token, "reference")
            && tokens
                .get(i + 1)
                .is_some_and(|next| is_keyword(source, next, "into"))
        {
            i = scan_and_push_reference_into_clause(
                b,
                children,
                source,
                tokens,
                i + 1,
                entry_end,
                &clause_starts,
            );
            continue;
        }
        if is_keyword(source, token, "index") {
            children.push(token_leaf(b, token));
            i = scan_and_push_expr_clause(
                b,
                children,
                source,
                tokens,
                i + 1,
                entry_end,
                Some(token),
                &clause_starts,
            );
            continue;
        }
        if is_keyword(source, token, "with") {
            children.push(token_leaf(b, token));
            i += 1;
            while i < entry_end {
                let current = &tokens[i];
                if clause_starts(tokens, i) {
                    break;
                }
                children.push(token_leaf(b, current));
                if current.kind == TokenKind::Eq {
                    let value_end = scan_read_table_key_value_end(source, tokens, i + 1, entry_end);
                    push_expr_child(b, children, source, tokens, i + 1, value_end, Some(current));
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

    true
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

/// Ends the row/line source expression before top-level `INTO`.
fn insert_internal_source_clause_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    token.kind == TokenKind::Ident && is_keyword(source, token, "into")
}

/// After `INTO` without `TABLE`, ends the internal-table target before tail or DB `VALUES` / `SET`.
fn insert_into_bare_itab_clause_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    if token.kind != TokenKind::Ident {
        return false;
    }
    is_keyword(source, token, "index")
        || is_keyword(source, token, "assigning")
        || is_keyword(source, token, "values")
        || is_keyword(source, token, "set")
        || (is_keyword(source, token, "reference")
            && tokens
                .get(idx + 1)
                .is_some_and(|next| is_keyword(source, next, "into")))
}

fn insert_into_table_tail_clause_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    token.kind == TokenKind::Ident
        && (is_keyword(source, token, "index")
            || is_keyword(source, token, "assigning")
            || (is_keyword(source, token, "reference")
                && tokens
                    .get(idx + 1)
                    .is_some_and(|next| is_keyword(source, next, "into"))))
}

fn insert_db_table_clause_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    token.kind == TokenKind::Ident
        && (is_keyword(source, token, "from")
            || is_keyword(source, token, "values")
            || is_keyword(source, token, "using")
            || is_keyword(source, token, "client")
            || is_keyword(source, token, "connection")
            || is_keyword(source, token, "accepting"))
}

fn insert_db_table_tail_clause_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    token.kind == TokenKind::Ident && is_keyword(source, token, "accepting")
}

fn insert_textpool_clause_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    token.kind == TokenKind::Ident
        && (is_keyword(source, token, "from") || is_keyword(source, token, "language"))
}

fn build_insert_textpool_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    insert_idx: usize,
    period_i: usize,
) -> NodeId {
    let insert_tok = &tokens[insert_idx];
    let textpool_idx = insert_idx + 1;
    let Some(from_idx) =
        find_top_level_keyword_index(source, tokens, textpool_idx + 1, period_i, "from")
    else {
        let children = token_children(b, tokens, insert_idx, period_i + 1);
        return b.branch(
            SyntaxKind::Error,
            insert_tok.range.start..tokens[period_i].range.end,
            &children,
        );
    };
    let language_idx =
        find_top_level_keyword_index(source, tokens, from_idx + 1, period_i, "language");

    let mut children = Vec::with_capacity(period_i - insert_idx + 1);
    children.push(token_leaf(b, insert_tok));
    children.push(token_leaf(b, &tokens[textpool_idx]));

    let clause_starts =
        |tokens: &[Token], i: usize| insert_textpool_clause_starts(source, tokens, i);
    scan_and_push_expr_clause(
        b,
        &mut children,
        source,
        tokens,
        textpool_idx + 1,
        from_idx,
        Some(&tokens[textpool_idx]),
        &clause_starts,
    );
    children.push(token_leaf(b, &tokens[from_idx]));
    let itab_end = language_idx.unwrap_or(period_i);
    scan_and_push_expr_clause(
        b,
        &mut children,
        source,
        tokens,
        from_idx + 1,
        itab_end,
        Some(&tokens[from_idx]),
        &clause_starts,
    );
    if let Some(language_idx) = language_idx {
        children.push(token_leaf(b, &tokens[language_idx]));
        scan_and_push_expr_clause(
            b,
            &mut children,
            source,
            tokens,
            language_idx + 1,
            period_i,
            Some(&tokens[language_idx]),
            &clause_starts,
        );
    }
    children.push(token_leaf(b, &tokens[period_i]));

    b.branch(
        SyntaxKind::InsertTextpoolStmt,
        insert_tok.range.start..tokens[period_i].range.end,
        &children,
    )
}

fn find_insert_into_db_table_target_end(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> Option<usize> {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut first_db_clause = None;
    let mut idx = start;

    while idx < end_exclusive {
        let token = &tokens[idx];
        if paren == 0 && bracket == 0 && brace == 0 && token.kind == TokenKind::Ident {
            if is_keyword(source, token, "values") {
                return Some(first_db_clause.unwrap_or(idx));
            }
            if is_keyword(source, token, "using")
                || is_keyword(source, token, "client")
                || is_keyword(source, token, "connection")
            {
                first_db_clause.get_or_insert(idx);
            } else if is_keyword(source, token, "index")
                || is_keyword(source, token, "assigning")
                || is_keyword(source, token, "set")
                || (is_keyword(source, token, "reference")
                    && tokens
                        .get(idx + 1)
                        .is_some_and(|next| is_keyword(source, next, "into")))
            {
                return None;
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
        idx += 1;
    }

    None
}

fn build_insert_db_table_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    insert_idx: usize,
    period_i: usize,
    into_idx: Option<usize>,
    target_start: usize,
    target_end: usize,
) -> NodeId {
    let insert_tok = &tokens[insert_idx];
    let mut children = Vec::with_capacity(period_i - insert_idx + 1);
    children.push(token_leaf(b, insert_tok));
    if let Some(into_idx) = into_idx {
        children.push(token_leaf(b, &tokens[into_idx]));
    }

    if let Some(source_node) = build_sql_data_source(b, source, tokens, target_start, target_end) {
        children.push(source_node);
    } else {
        push_token_children(b, &mut children, tokens, target_start, target_end);
    }

    let clause_starts =
        |tokens: &[Token], i: usize| insert_db_table_clause_starts(source, tokens, i);
    let tail_clause =
        |tokens: &[Token], i: usize| insert_db_table_tail_clause_starts(source, tokens, i);

    let mut i = target_end;
    while i < period_i {
        let token = &tokens[i];
        if is_keyword(source, token, "using")
            && tokens
                .get(i + 1)
                .is_some_and(|next| is_keyword(source, next, "client"))
        {
            children.push(token_leaf(b, token));
            children.push(token_leaf(b, &tokens[i + 1]));
            i = scan_and_push_sql_host_or_expr_clause(
                b,
                &mut children,
                source,
                tokens,
                i + 2,
                period_i,
                Some(&tokens[i + 1]),
                &clause_starts,
            );
            continue;
        }
        if is_keyword(source, token, "connection") {
            children.push(token_leaf(b, token));
            i = scan_and_push_sql_host_or_expr_clause(
                b,
                &mut children,
                source,
                tokens,
                i + 1,
                period_i,
                Some(token),
                &clause_starts,
            );
            continue;
        }
        if is_keyword(source, token, "client") {
            children.push(token_leaf(b, token));
            i += 1;
            if i < period_i && is_keyword(source, &tokens[i], "specified") {
                children.push(token_leaf(b, &tokens[i]));
                i += 1;
            }
            continue;
        }
        if is_keyword(source, token, "values") {
            children.push(token_leaf(b, token));
            i = scan_and_push_sql_host_or_expr_clause(
                b,
                &mut children,
                source,
                tokens,
                i + 1,
                period_i,
                Some(token),
                &tail_clause,
            );
            continue;
        }
        if is_keyword(source, token, "from") {
            children.push(token_leaf(b, token));
            i += 1;
            if tokens
                .get(i)
                .is_some_and(|token| is_keyword(source, token, "table"))
            {
                children.push(token_leaf(b, &tokens[i]));
                i += 1;
            }
            i = scan_and_push_sql_host_or_expr_clause(
                b,
                &mut children,
                source,
                tokens,
                i,
                period_i,
                Some(token),
                &tail_clause,
            );
            continue;
        }
        children.push(token_leaf(b, token));
        i += 1;
    }

    children.push(token_leaf(b, &tokens[period_i]));
    b.branch(
        SyntaxKind::InsertDbTableStmt,
        insert_tok.range.start..tokens[period_i].range.end,
        &children,
    )
}

fn modify_clause_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    token.kind == TokenKind::Ident
        && (is_keyword(source, token, "from")
            || is_keyword(source, token, "index")
            || is_keyword(source, token, "transporting")
            || is_keyword(source, token, "where"))
}

fn update_clause_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    if token.kind != TokenKind::Ident {
        return false;
    }
    is_keyword(source, token, "set")
        || is_keyword(source, token, "from")
        || is_keyword(source, token, "where")
        || is_keyword(source, token, "using")
        || is_keyword(source, token, "connection")
        || is_keyword(source, token, "client")
}

fn find_top_level_clause_index(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
    keywords: &[&str],
) -> Option<usize> {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut idx = start;
    while idx < end_exclusive {
        let token = &tokens[idx];
        if paren == 0
            && bracket == 0
            && brace == 0
            && token.kind == TokenKind::Ident
            && keywords
                .iter()
                .any(|keyword| is_keyword(source, token, keyword))
        {
            return Some(idx);
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
    None
}

fn scan_update_set_assignment_end(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> usize {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut idx = start;
    while idx < end_exclusive {
        let token = &tokens[idx];
        if paren == 0 && bracket == 0 && brace == 0 {
            if token.kind == TokenKind::Comma {
                return idx;
            }
            if update_clause_starts(source, tokens, idx) {
                return idx;
            }
            if idx > start
                && tokens
                    .get(idx + 1)
                    .is_some_and(|next| matches!(next.kind, TokenKind::Eq | TokenKind::QuestionEq))
                && token.kind == TokenKind::Ident
            {
                return idx;
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
        idx += 1;
    }
    idx
}

fn delete_clause_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    token.kind == TokenKind::Ident
        && (is_keyword(source, token, "from")
            || is_keyword(source, token, "where")
            || is_keyword(source, token, "index")
            || is_keyword(source, token, "using")
            || is_keyword(source, token, "comparing"))
}

fn delete_stmt_kind(source: &str, tokens: &[Token], start: usize, period_i: usize) -> SyntaxKind {
    if tokens
        .get(start)
        .is_some_and(|token| is_keyword(source, token, "table"))
    {
        return SyntaxKind::DeleteStmt;
    }
    let Some(from_idx) = find_top_level_keyword_index(source, tokens, start, period_i, "from")
    else {
        return SyntaxKind::DeleteStmt;
    };
    let table_idx = skip_trivia(tokens, from_idx + 1);
    if tokens
        .get(table_idx)
        .is_some_and(|token| is_keyword(source, token, "table"))
    {
        SyntaxKind::DeleteDbTableStmt
    } else {
        SyntaxKind::DeleteStmt
    }
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

#[derive(Clone, Copy)]
enum EnhancementEndKind {
    Section,
    Enhancement,
}

fn enhancement_end_keyword_text(kind: EnhancementEndKind) -> &'static str {
    match kind {
        EnhancementEndKind::Section => "END-ENHANCEMENT-SECTION",
        EnhancementEndKind::Enhancement => "ENDENHANCEMENT",
    }
}

fn enhancement_end_keyword_end(
    source: &str,
    tokens: &[Token],
    idx: usize,
    kind: EnhancementEndKind,
) -> Option<usize> {
    match kind {
        EnhancementEndKind::Section => {
            match_hyphenated_keyword(source, tokens, idx, &["end", "enhancement", "section"])
        }
        EnhancementEndKind::Enhancement => tokens
            .get(idx)
            .is_some_and(|token| is_keyword(source, token, "endenhancement"))
            .then_some(idx + 1),
    }
}

fn recover_skip_after_enhancement_end(
    source: &str,
    tokens: &[Token],
    mut idx: usize,
    kind: EnhancementEndKind,
) -> usize {
    while idx < tokens.len() {
        if let Some(end) = enhancement_end_keyword_end(source, tokens, idx, kind) {
            let period_idx = skip_trivia(tokens, end);
            if tokens.get(period_idx).map(|token| token.kind) == Some(TokenKind::Period) {
                return period_idx + 1;
            }
            return end;
        }
        idx += 1;
    }
    tokens.len()
}

fn parse_body_until_enhancement_end(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    mut idx: usize,
    errors: &mut Vec<crate::ParseError>,
    kind: EnhancementEndKind,
) -> (Vec<NodeId>, usize) {
    let mut nodes = Vec::new();
    loop {
        let boundary_idx = skip_trivia(tokens, idx);
        if enhancement_end_keyword_end(source, tokens, boundary_idx, kind).is_some() {
            break;
        }
        if idx >= tokens.len() || tokens[idx].kind == TokenKind::Eof {
            break;
        }
        let (node, next) = crate::parse_file_level_item(b, source, tokens, idx, errors);
        nodes.push(node);
        idx = crate::block_helpers::ensure_forward_progress(tokens, idx, next);
    }
    (nodes, idx)
}

fn parse_enhancement_end_keyword(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    start_tok: &Token,
    kind: EnhancementEndKind,
    errors: &mut Vec<crate::ParseError>,
) -> (Vec<NodeId>, usize, usize) {
    let end_text = enhancement_end_keyword_text(kind);
    let end_idx = skip_trivia(tokens, idx);
    let Some(end_tok) = tokens.get(end_idx) else {
        errors.push(crate::ParseError {
            message: format!("syntax error: expected {end_text}"),
            range: start_tok.range.clone(),
        });
        return (Vec::new(), tokens.len(), start_tok.range.end);
    };
    let Some(end_parts_end) = enhancement_end_keyword_end(source, tokens, end_idx, kind) else {
        errors.push(crate::ParseError {
            message: format!("syntax error: expected {end_text}"),
            range: start_tok.range.start..end_tok.range.end,
        });
        let recover = recover_skip_after_enhancement_end(source, tokens, idx, kind);
        return (Vec::new(), recover, end_tok.range.end);
    };

    let mut children = token_children(b, tokens, end_idx, end_parts_end);
    let period_idx = skip_trivia(tokens, end_parts_end);
    let Some(period_tok) = tokens.get(period_idx) else {
        let end_pos = children
            .last()
            .copied()
            .map(|node| b.span(node).end)
            .unwrap_or(end_tok.range.end);
        errors.push(crate::ParseError {
            message: format!("syntax error: expected '.' after {end_text}"),
            range: end_tok.range.start..end_pos,
        });
        let recover = recover_skip_after_enhancement_end(source, tokens, end_idx, kind);
        return (children, recover, end_pos);
    };
    if period_tok.kind != TokenKind::Period {
        errors.push(crate::ParseError {
            message: format!("syntax error: expected '.' after {end_text}"),
            range: end_tok.range.start..period_tok.range.end,
        });
        let recover = recover_skip_after_enhancement_end(source, tokens, end_idx, kind);
        return (children, recover, period_tok.range.end);
    }

    children.push(token_leaf(b, period_tok));
    (children, period_idx + 1, period_tok.range.end)
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
        || is_keyword(source, token, "raising")
}

fn function_header_section_keyword(source: &str, token: &Token) -> bool {
    is_keyword(source, token, "importing")
        || is_keyword(source, token, "exporting")
        || is_keyword(source, token, "changing")
        || is_keyword(source, token, "tables")
        || is_keyword(source, token, "raising")
        || is_keyword(source, token, "exceptions")
}

fn form_header_section_is_raising(source: &str, token: &Token) -> bool {
    is_keyword(source, token, "raising")
}

fn function_header_section_is_raising(source: &str, token: &Token) -> bool {
    is_keyword(source, token, "raising")
}

fn function_header_section_is_exceptions(source: &str, token: &Token) -> bool {
    is_keyword(source, token, "exceptions")
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

fn function_header_starts_param(source: &str, tokens: &[Token], idx: usize, end: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    if token.kind != TokenKind::Ident || function_header_section_keyword(source, token) {
        return false;
    }
    if is_keyword(source, token, "value") || is_keyword(source, token, "reference") {
        if idx + 3 >= end {
            return false;
        }
        return tokens.get(idx + 1).map(|t| t.kind) == Some(TokenKind::LParen)
            && tokens.get(idx + 2).map(|t| t.kind) == Some(TokenKind::Ident)
            && tokens.get(idx + 3).map(|t| t.kind) == Some(TokenKind::RParen);
    }
    true
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

fn skip_function_header_type_expression(
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
            TokenKind::LParen | TokenKind::LBracket | TokenKind::LBrace => {
                depth += 1;
                idx += 1;
            }
            TokenKind::RParen | TokenKind::RBracket | TokenKind::RBrace => {
                depth -= 1;
                idx += 1;
            }
            TokenKind::Period if depth == 0 => return idx,
            _ if depth == 0 && function_header_section_keyword(source, token) => return idx,
            _ if depth == 0
                && (is_keyword(source, token, "optional")
                    || is_keyword(source, token, "default")) =>
            {
                return idx;
            }
            _ if depth == 0
                && token.has_newline_before()
                && function_header_starts_param(source, tokens, idx, end) =>
            {
                return idx;
            }
            _ => idx += 1,
        }
    }
    idx
}

fn consume_raising_type_ref_end(tokens: &[Token], idx: usize, end: usize) -> Option<usize> {
    let first = tokens.get(idx)?;
    if first.kind != TokenKind::Ident {
        return None;
    }
    let mut i = idx + 1;
    while i + 1 < end {
        let op = &tokens[i];
        let next = &tokens[i + 1];
        if matches!(
            op.kind,
            TokenKind::Minus | TokenKind::Arrow | TokenKind::Tilde | TokenKind::FatArrow
        ) && next.kind == TokenKind::Ident
        {
            i += 2;
        } else {
            break;
        }
    }
    Some(i)
}

fn build_raising_entry_node(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    end: usize,
    kind: SyntaxKind,
) -> Option<(NodeId, usize)> {
    let token = tokens.get(idx)?;
    let start = token.range.start;

    if is_keyword(source, token, "resumable") {
        let lparen_idx = idx + 1;
        if tokens.get(lparen_idx).map(|token| token.kind) != Some(TokenKind::LParen) {
            return None;
        }
        let rparen_idx =
            find_matching_delim(tokens, lparen_idx, TokenKind::LParen, TokenKind::RParen)?;
        if rparen_idx >= end || idx + 2 >= rparen_idx {
            return None;
        }
        let children = vec![
            token_leaf(b, &tokens[idx]),
            token_leaf(b, &tokens[lparen_idx]),
            build_type_ref_node(b, source, &tokens[idx + 2..rparen_idx]),
            token_leaf(b, &tokens[rparen_idx]),
        ];
        let node = b.branch(kind, start..tokens[rparen_idx].range.end, &children);
        return Some((node, rparen_idx + 1));
    }

    let type_end = consume_raising_type_ref_end(tokens, idx, end)?;
    let child = build_type_ref_node(b, source, &tokens[idx..type_end]);
    let node = b.branch(kind, start..tokens[type_end - 1].range.end, &[child]);
    Some((node, type_end))
}

fn build_form_header_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let mut children = vec![token_leaf(b, &tokens[idx])];
    let mut saw_form_name = false;
    let mut i = idx + 1;
    while i <= period_i {
        let token = &tokens[i];
        if token.kind == TokenKind::Comment {
            children.push(token_leaf(b, token));
            i += 1;
            continue;
        }

        if !saw_form_name && let Some((name, next_i)) = parse_inline_name(b, tokens, i) {
            children.push(name);
            saw_form_name = true;
            i = next_i;
            continue;
        }

        if is_keyword(source, token, "tables")
            || is_keyword(source, token, "using")
            || is_keyword(source, token, "changing")
            || is_keyword(source, token, "raising")
        {
            let (section, next_i) = build_form_param_section_node(b, source, tokens, i, period_i);
            children.push(section);
            i = next_i;
            continue;
        }

        children.push(token_leaf(b, token));
        i += 1;
    }
    children
}

fn build_form_param_section_node(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> (NodeId, usize) {
    let mut children = vec![token_leaf(b, &tokens[idx])];
    let raising_section = form_header_section_is_raising(source, &tokens[idx]);
    let mut i = idx + 1;
    while i <= period_i {
        let token = &tokens[i];
        if token.kind == TokenKind::Period || form_header_section_keyword(source, token) {
            break;
        }
        if token.kind == TokenKind::Comment {
            children.push(token_leaf(b, token));
            i += 1;
            continue;
        }
        if raising_section
            && let Some((param, next_i)) =
                build_raising_entry_node(b, source, tokens, i, period_i + 1, SyntaxKind::FormParam)
        {
            children.push(param);
            i = next_i;
            continue;
        }
        if let Some((param, next_i)) = build_form_param_node(b, source, tokens, i, period_i + 1) {
            children.push(param);
            i = next_i;
            continue;
        }
        children.push(token_leaf(b, token));
        i += 1;
    }

    let end = children
        .last()
        .copied()
        .map(|id| b.span(id).end)
        .unwrap_or(tokens[idx].range.end);
    (
        b.branch(
            SyntaxKind::FormParamSection,
            tokens[idx].range.start..end,
            &children,
        ),
        i,
    )
}

fn build_form_param_node(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    end: usize,
) -> Option<(NodeId, usize)> {
    let mut children = Vec::new();
    let start = tokens.get(idx)?.range.start;
    let mut i = idx;

    if is_keyword(source, tokens.get(i)?, "value")
        || is_keyword(source, tokens.get(i)?, "reference")
    {
        let lparen_idx = i + 1;
        let (name, next_i) = parse_inline_name(b, tokens, i + 2)?;
        let rparen_idx = next_i;
        if tokens.get(lparen_idx).map(|token| token.kind) != Some(TokenKind::LParen)
            || tokens.get(rparen_idx).map(|token| token.kind) != Some(TokenKind::RParen)
        {
            return None;
        }
        children.push(token_leaf(b, &tokens[i]));
        children.push(token_leaf(b, &tokens[lparen_idx]));
        children.push(name);
        children.push(token_leaf(b, &tokens[rparen_idx]));
        i = rparen_idx + 1;
    } else {
        let (name, next_i) = parse_inline_name(b, tokens, i)?;
        children.push(name);
        i = next_i;
    }

    let mut j = i;
    while j < end && tokens[j].kind == TokenKind::Comment {
        children.push(token_leaf(b, &tokens[j]));
        j += 1;
    }

    if j < end
        && (is_keyword(source, &tokens[j], "type")
            || is_keyword(source, &tokens[j], "like")
            || is_keyword(source, &tokens[j], "structure"))
    {
        children.push(token_leaf(b, &tokens[j]));
        j += 1;
        while j < end && tokens[j].kind == TokenKind::Comment {
            children.push(token_leaf(b, &tokens[j]));
            j += 1;
        }
        let expr_end = skip_form_header_type_expression(source, tokens, j, end);
        if j < expr_end {
            children.push(build_type_ref_node(b, source, &tokens[j..expr_end]));
            i = expr_end;
        } else {
            i = j;
        }
    } else {
        i = j;
    }

    let end_range = children
        .last()
        .copied()
        .map(|id| b.span(id).end)
        .unwrap_or(start);
    Some((
        b.branch(SyntaxKind::FormParam, start..end_range, &children),
        i,
    ))
}

fn build_function_header_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let mut children = vec![token_leaf(b, &tokens[idx])];
    let mut saw_name = false;
    let mut i = idx + 1;
    while i <= period_i {
        let token = &tokens[i];
        if token.kind == TokenKind::Comment {
            children.push(token_leaf(b, token));
            i += 1;
            continue;
        }

        if !saw_name && let Some((name, next_i)) = parse_inline_name(b, tokens, i) {
            children.push(name);
            saw_name = true;
            i = next_i;
            continue;
        }

        if function_header_section_keyword(source, token) {
            let (section, next_i) =
                build_function_param_section_node(b, source, tokens, i, period_i);
            children.push(section);
            i = next_i;
            continue;
        }

        children.push(token_leaf(b, token));
        i += 1;
    }
    children
}

fn build_function_param_section_node(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> (NodeId, usize) {
    let mut children = vec![token_leaf(b, &tokens[idx])];
    let raising_section = function_header_section_is_raising(source, &tokens[idx]);
    let exceptions_section = function_header_section_is_exceptions(source, &tokens[idx]);
    let mut i = idx + 1;
    while i <= period_i {
        let token = &tokens[i];
        if token.kind == TokenKind::Period || function_header_section_keyword(source, token) {
            break;
        }
        if token.kind == TokenKind::Comment {
            children.push(token_leaf(b, token));
            i += 1;
            continue;
        }
        if raising_section
            && let Some((param, next_i)) = build_raising_entry_node(
                b,
                source,
                tokens,
                i,
                period_i + 1,
                SyntaxKind::FunctionParam,
            )
        {
            children.push(param);
            i = next_i;
            continue;
        }
        if exceptions_section && let Some((name, next_i)) = parse_inline_name(b, tokens, i) {
            let node = b.branch(
                SyntaxKind::FunctionParam,
                b.span(name).start..b.span(name).end,
                &[name],
            );
            children.push(node);
            i = next_i;
            continue;
        }
        let can_start_param = i == idx + 1 || token.has_newline_before();
        if can_start_param
            && let Some((param, next_i)) =
                build_function_param_node(b, source, tokens, i, period_i + 1)
        {
            children.push(param);
            i = next_i;
            continue;
        }
        children.push(token_leaf(b, token));
        i += 1;
    }

    let end = children
        .last()
        .copied()
        .map(|id| b.span(id).end)
        .unwrap_or(tokens[idx].range.end);
    (
        b.branch(
            SyntaxKind::FunctionParamSection,
            tokens[idx].range.start..end,
            &children,
        ),
        i,
    )
}

fn build_function_param_node(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    end: usize,
) -> Option<(NodeId, usize)> {
    if !function_header_starts_param(source, tokens, idx, end) {
        return None;
    }

    let mut children = Vec::new();
    let start = tokens.get(idx)?.range.start;
    let mut i = idx;

    if is_keyword(source, tokens.get(i)?, "value")
        || is_keyword(source, tokens.get(i)?, "reference")
    {
        let lparen_idx = i + 1;
        let (name, next_i) = parse_inline_name(b, tokens, i + 2)?;
        let rparen_idx = next_i;
        if tokens.get(lparen_idx).map(|token| token.kind) != Some(TokenKind::LParen)
            || tokens.get(rparen_idx).map(|token| token.kind) != Some(TokenKind::RParen)
        {
            return None;
        }
        children.push(token_leaf(b, &tokens[i]));
        children.push(token_leaf(b, &tokens[lparen_idx]));
        children.push(name);
        children.push(token_leaf(b, &tokens[rparen_idx]));
        i = rparen_idx + 1;
    } else {
        let (name, next_i) = parse_inline_name(b, tokens, i)?;
        children.push(name);
        i = next_i;
    }

    while i < end && tokens[i].kind == TokenKind::Comment {
        children.push(token_leaf(b, &tokens[i]));
        i += 1;
    }

    if i < end
        && (is_keyword(source, &tokens[i], "type")
            || is_keyword(source, &tokens[i], "like")
            || is_keyword(source, &tokens[i], "structure"))
    {
        children.push(token_leaf(b, &tokens[i]));
        i += 1;
        while i < end && tokens[i].kind == TokenKind::Comment {
            children.push(token_leaf(b, &tokens[i]));
            i += 1;
        }
        let expr_end = skip_function_header_type_expression(source, tokens, i, end);
        if i < expr_end {
            children.push(build_type_ref_node(b, source, &tokens[i..expr_end]));
            i = expr_end;
        }
    }

    let mut depth = 0i32;
    while i < end {
        let token = &tokens[i];
        match token.kind {
            TokenKind::LParen | TokenKind::LBracket | TokenKind::LBrace => depth += 1,
            TokenKind::RParen | TokenKind::RBracket | TokenKind::RBrace => depth -= 1,
            _ => {}
        }
        if depth == 0 {
            if token.kind == TokenKind::Period || function_header_section_keyword(source, token) {
                break;
            }
            if token.has_newline_before() && function_header_starts_param(source, tokens, i, end) {
                break;
            }
        }
        children.push(token_leaf(b, token));
        i += 1;
    }

    let end_range = children
        .last()
        .copied()
        .map(|id| b.span(id).end)
        .unwrap_or(start);
    Some((
        b.branch(SyntaxKind::FunctionParam, start..end_range, &children),
        i,
    ))
}

const CLASS_HEADER_TYPE_REF_STOP_KEYWORDS: &[&str] = &[
    "abstract",
    "create",
    "final",
    "for",
    "friends",
    "global",
    "local",
    "private",
    "protected",
    "public",
    "shared",
    "testing",
];

fn build_class_header_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let mut children = Vec::with_capacity(period_i - idx + 1);
    let mut saw_name = false;
    let mut i = idx;
    while i <= period_i {
        let token = &tokens[i];
        if token.kind == TokenKind::Comment {
            children.push(token_leaf(b, token));
            i += 1;
            continue;
        }
        if !saw_name
            && i > idx
            && let Some((name, next_i)) = parse_inline_name(b, tokens, i)
        {
            children.push(name);
            saw_name = true;
            i = next_i;
            continue;
        }
        if is_keyword(source, token, "implementation") {
            let leaf = token_leaf(b, token);
            children.push(b.branch(
                SyntaxKind::ClassImplementationMarker,
                token.range.clone(),
                &[leaf],
            ));
            i += 1;
            continue;
        }
        if is_keyword(source, token, "inheriting") {
            let from_idx = skip_trivia(tokens, i + 1);
            let expr_start = skip_trivia(tokens, from_idx + 1);
            if from_idx <= period_i
                && tokens
                    .get(from_idx)
                    .is_some_and(|from| is_keyword(source, from, "from"))
                && let Some((type_ref, next_i)) = parse_type_ref_tokens(
                    b,
                    source,
                    tokens,
                    expr_start,
                    CLASS_HEADER_TYPE_REF_STOP_KEYWORDS,
                )
            {
                let mut clause_children = Vec::new();
                for token in &tokens[i..expr_start] {
                    clause_children.push(token_leaf(b, token));
                }
                clause_children.push(type_ref);
                let end = clause_children
                    .last()
                    .copied()
                    .map(|id| b.span(id).end)
                    .unwrap_or(token.range.end);
                children.push(b.branch(
                    SyntaxKind::ClassInheritanceClause,
                    token.range.start..end,
                    &clause_children,
                ));
                i = next_i;
                continue;
            }
        }
        children.push(token_leaf(b, token));
        i += 1;
    }
    children
}

fn build_interface_header_children(
    b: &mut SyntaxTreeBuilder,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let mut children = Vec::with_capacity(period_i - idx + 1);
    let mut saw_name = false;
    let mut i = idx;
    while i <= period_i {
        let token = &tokens[i];
        if token.kind == TokenKind::Comment {
            children.push(token_leaf(b, token));
            i += 1;
            continue;
        }
        if !saw_name
            && i > idx
            && let Some((name, next_i)) = parse_inline_name(b, tokens, i)
        {
            children.push(name);
            saw_name = true;
            i = next_i;
            continue;
        }
        children.push(token_leaf(b, token));
        i += 1;
    }
    children
}

fn build_method_header_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Vec<NodeId> {
    let mut children = vec![token_leaf(b, &tokens[idx])];
    let mut i = idx + 1;
    while i < period_i && tokens[i].kind == TokenKind::Comment {
        children.push(token_leaf(b, &tokens[i]));
        i += 1;
    }
    if i < period_i
        && let Some((target, next_i)) =
            build_method_decl_target_node(b, source, tokens, i, period_i)
    {
        children.push(target);
        i = next_i;
    }
    while i <= period_i {
        children.push(token_leaf(b, &tokens[i]));
        i += 1;
    }
    children
}

fn build_method_decl_target_node(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Option<(NodeId, usize)> {
    let token = tokens.get(idx)?;
    if token.kind != TokenKind::Ident {
        return None;
    }

    let mut children = Vec::new();
    let start = token.range.start;
    let mut i = idx;
    if tokens.get(i + 1).map(|token| token.kind) == Some(TokenKind::Tilde)
        && tokens.get(i + 2).map(|token| token.kind) == Some(TokenKind::Ident)
    {
        children.push(build_type_ref_node(b, source, &tokens[i..i + 1]));
        children.push(token_leaf(b, &tokens[i + 1]));
        let (member, next_i) = parse_inline_name(b, tokens, i + 2)?;
        children.push(member);
        i = next_i;
    } else {
        let (member, next_i) = parse_inline_name(b, tokens, i)?;
        children.push(member);
        i = next_i;
    }
    let end = children
        .last()
        .copied()
        .map(|id| b.span(id).end)
        .unwrap_or(start);
    Some((
        b.branch(SyntaxKind::MethodDeclTarget, start..end, &children),
        i.min(period_i),
    ))
}

fn method_header_has_keyword_sequence(
    source: &str,
    tokens: &[Token],
    start: usize,
    end: usize,
    sequence: &[&str],
) -> bool {
    let mut i = start;
    while i < end {
        if tokens[i].kind == TokenKind::Comment {
            i += 1;
            continue;
        }

        let mut j = i;
        let mut matched = true;
        for expected in sequence {
            while j < end && tokens[j].kind == TokenKind::Comment {
                j += 1;
            }
            if j >= end || !is_keyword(source, &tokens[j], expected) {
                matched = false;
                break;
            }
            j += 1;
        }
        if matched {
            return true;
        }
        i += 1;
    }
    false
}

fn method_header_is_amdp(source: &str, tokens: &[Token], idx: usize, period_i: usize) -> bool {
    let has_database_function = method_header_has_keyword_sequence(
        source,
        tokens,
        idx + 1,
        period_i,
        &["by", "database", "function"],
    );
    let has_database_procedure = method_header_has_keyword_sequence(
        source,
        tokens,
        idx + 1,
        period_i,
        &["by", "database", "procedure"],
    );
    let has_sqlscript = method_header_has_keyword_sequence(
        source,
        tokens,
        idx + 1,
        period_i,
        &["language", "sqlscript"],
    );

    (has_database_function || has_database_procedure) && has_sqlscript
}

fn find_sqlscript_island_end(source: &str, tokens: &[Token], mut idx: usize) -> usize {
    while idx < tokens.len() {
        let token = &tokens[idx];
        if token.kind == TokenKind::Eof {
            return idx;
        }
        if is_keyword(source, token, "endmethod") {
            let j = skip_trivia(tokens, idx + 1);
            if tokens
                .get(j)
                .is_some_and(|next| next.kind == TokenKind::Period)
            {
                return idx;
            }
        }
        idx += 1;
    }
    tokens.len()
}

fn parse_sqlscript_island_until_endmethod(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
) -> (Vec<NodeId>, usize) {
    let end = find_sqlscript_island_end(source, tokens, idx);
    if idx >= end {
        return (Vec::new(), end);
    }

    let mut children = Vec::with_capacity(end - idx);
    for token in &tokens[idx..end] {
        children.push(token_leaf(b, token));
    }
    let range = tokens[idx].range.start..tokens[end - 1].range.end;
    let island = b.branch(SyntaxKind::SqlScriptIsland, range, &children);
    (vec![island], end)
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

fn event_block_header_end(source: &str, tokens: &[Token], idx: usize) -> Option<usize> {
    let start_tok = tokens.get(idx)?;
    if start_tok.kind != TokenKind::Ident {
        return None;
    }

    for lead in EVENT_BLOCK_LEADS {
        match lead {
            EventBlockLead::Single(keyword) => {
                if is_keyword(source, start_tok, keyword) {
                    return Some(idx + 1);
                }
            }
            EventBlockLead::Hyphenated(parts) => {
                if let Some(next) = match_hyphenated_keyword(source, tokens, idx, parts) {
                    return Some(next);
                }
            }
            EventBlockLead::AtHyphenated(parts) => {
                if is_keyword(source, start_tok, "at")
                    && let Some(next) = match_hyphenated_keyword(source, tokens, idx + 1, parts)
                {
                    return Some(next);
                }
            }
        }
    }

    None
}

fn macro_end_keyword_end(source: &str, tokens: &[Token], idx: usize) -> Option<usize> {
    match_hyphenated_keyword(source, tokens, idx, MACRO_END_OF_DEFINITION)
}

fn find_macro_end_keyword(source: &str, tokens: &[Token], mut idx: usize) -> Option<usize> {
    while idx < tokens.len() {
        if macro_end_keyword_end(source, tokens, idx).is_some() {
            return Some(idx);
        }
        idx += 1;
    }
    None
}

pub fn try_parse_report_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let tok = tokens.get(idx)?;
    if !is_keyword(source, tok, "report") && !is_keyword(source, tok, "program") {
        return None;
    }
    parse_simple_keyword_stmt(
        b,
        source,
        tokens,
        idx,
        SyntaxKind::ReportStmt,
        if is_keyword(source, tok, "program") {
            "program"
        } else {
            "report"
        },
        errors,
        "syntax error: expected '.' after REPORT/PROGRAM",
    )
}

pub fn try_parse_include_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let tok = tokens.get(idx)?;
    if !is_keyword(source, tok, "include") {
        return None;
    }
    if tokens.get(idx + 1).is_some_and(|next| {
        is_keyword(source, next, "type") || is_keyword(source, next, "structure")
    }) {
        return None;
    }
    Some(parse_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        idx + 1,
        tok,
        "syntax error: expected '.' after INCLUDE",
        errors,
        next_after_unterminated_scan,
        |b, period_i, _errors| {
            let children = build_include_stmt_children(b, tokens, idx, period_i);
            let node = b.branch(
                SyntaxKind::IncludeStmt,
                tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        },
    ))
}

pub fn try_parse_write_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let write_tok = tokens.get(idx)?;
    if !is_keyword(source, write_tok, "write") {
        return None;
    }
    Some(parse_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        idx + 1,
        write_tok,
        "syntax error: expected '.' after WRITE statement",
        errors,
        next_after_unterminated_scan,
        |b, period_i, _errors| {
            let mut children = vec![token_leaf(b, write_tok)];
            let mut i = idx + 1;
            while i < period_i {
                if let Some(next) =
                    push_write_separator_or_position_tokens(b, &mut children, tokens, i, period_i)
                {
                    i = next;
                    continue;
                }
                let expr_end = scan_until_clause(tokens, i, period_i, |tokens, at| {
                    matches!(
                        tokens[at].kind,
                        TokenKind::Colon | TokenKind::Slash | TokenKind::Comma
                    )
                });
                push_expr_child(
                    b,
                    &mut children,
                    source,
                    tokens,
                    i,
                    expr_end,
                    Some(write_tok),
                );
                i = expr_end;
            }
            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::WriteStmt,
                write_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        },
    ))
}

pub fn try_parse_concatenate_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let concat_tok = tokens.get(idx)?;
    if !is_keyword(source, concat_tok, "concatenate") {
        return None;
    }
    Some(parse_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        idx + 1,
        concat_tok,
        "syntax error: expected '.' after CONCATENATE statement",
        errors,
        next_after_unterminated_scan,
        |b, period_i, _errors| {
            let mut children = vec![token_leaf(b, concat_tok)];
            if tokens
                .get(idx + 1)
                .is_some_and(|token| token.kind == TokenKind::Colon)
            {
                children.push(token_leaf(b, &tokens[idx + 1]));
                let mut cursor = idx + 2;
                let mut parsed_entry = false;
                while cursor < period_i {
                    while cursor < period_i && tokens[cursor].kind == TokenKind::Comment {
                        children.push(token_leaf(b, &tokens[cursor]));
                        cursor += 1;
                    }
                    if cursor >= period_i {
                        break;
                    }
                    let entry_end =
                        find_top_level_token_kind(tokens, cursor, period_i, TokenKind::Comma)
                            .unwrap_or(period_i);
                    let entry_start = skip_trivia(tokens, cursor);
                    if entry_start >= entry_end
                        || !push_concatenate_entry_children(
                            b,
                            &mut children,
                            source,
                            tokens,
                            entry_start,
                            entry_end,
                        )
                    {
                        let raw = token_children(b, tokens, idx, period_i + 1);
                        let node = b.branch(
                            SyntaxKind::Error,
                            concat_tok.range.start..tokens[period_i].range.end,
                            &raw,
                        );
                        return (node, period_i + 1);
                    }
                    parsed_entry = true;
                    if entry_end < period_i && tokens[entry_end].kind == TokenKind::Comma {
                        children.push(token_leaf(b, &tokens[entry_end]));
                    }
                    cursor = entry_end + 1;
                }
                if !parsed_entry {
                    let raw = token_children(b, tokens, idx, period_i + 1);
                    let node = b.branch(
                        SyntaxKind::Error,
                        concat_tok.range.start..tokens[period_i].range.end,
                        &raw,
                    );
                    return (node, period_i + 1);
                }
            } else if !push_concatenate_entry_children(
                b,
                &mut children,
                source,
                tokens,
                idx + 1,
                period_i,
            ) {
                let raw = token_children(b, tokens, idx, period_i + 1);
                let node = b.branch(
                    SyntaxKind::Error,
                    concat_tok.range.start..tokens[period_i].range.end,
                    &raw,
                );
                return (node, period_i + 1);
            }

            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::ConcatenateStmt,
                concat_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        },
    ))
}

pub fn try_parse_split_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let split_tok = tokens.get(idx)?;
    if !is_keyword(source, split_tok, "split") {
        return None;
    }
    Some(parse_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        idx + 1,
        split_tok,
        "syntax error: expected '.' after SPLIT statement",
        errors,
        next_after_unterminated_scan,
        |b, period_i, _errors| {
            let mut children = vec![token_leaf(b, split_tok)];

            if tokens
                .get(idx + 1)
                .is_some_and(|token| token.kind == TokenKind::Colon)
            {
                children.push(token_leaf(b, &tokens[idx + 1]));
                let mut cursor = idx + 2;
                let mut parsed_entry = false;
                while cursor < period_i {
                    while cursor < period_i && tokens[cursor].kind == TokenKind::Comment {
                        children.push(token_leaf(b, &tokens[cursor]));
                        cursor += 1;
                    }
                    if cursor >= period_i {
                        break;
                    }
                    let entry_end =
                        find_top_level_token_kind(tokens, cursor, period_i, TokenKind::Comma)
                            .unwrap_or(period_i);
                    let entry_start = skip_trivia(tokens, cursor);
                    if entry_start >= entry_end
                        || !push_split_entry_children(
                            b,
                            &mut children,
                            source,
                            tokens,
                            entry_start,
                            entry_end,
                        )
                    {
                        let raw = token_children(b, tokens, idx, period_i + 1);
                        let node = b.branch(
                            SyntaxKind::Error,
                            split_tok.range.start..tokens[period_i].range.end,
                            &raw,
                        );
                        return (node, period_i + 1);
                    }
                    parsed_entry = true;
                    if entry_end < period_i && tokens[entry_end].kind == TokenKind::Comma {
                        children.push(token_leaf(b, &tokens[entry_end]));
                    }
                    cursor = entry_end + 1;
                }
                if !parsed_entry {
                    let raw = token_children(b, tokens, idx, period_i + 1);
                    let node = b.branch(
                        SyntaxKind::Error,
                        split_tok.range.start..tokens[period_i].range.end,
                        &raw,
                    );
                    return (node, period_i + 1);
                }
            } else if !push_split_entry_children(
                b,
                &mut children,
                source,
                tokens,
                idx + 1,
                period_i,
            ) {
                let raw = token_children(b, tokens, idx, period_i + 1);
                let node = b.branch(
                    SyntaxKind::Error,
                    split_tok.range.start..tokens[period_i].range.end,
                    &raw,
                );
                return (node, period_i + 1);
            }

            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::SplitStmt,
                split_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        },
    ))
}

pub fn try_parse_condense_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let condense_tok = tokens.get(idx)?;
    if !is_keyword(source, condense_tok, "condense") {
        return None;
    }
    Some(parse_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        idx + 1,
        condense_tok,
        "syntax error: expected '.' after CONDENSE statement",
        errors,
        next_after_unterminated_scan,
        |b, period_i, errors| {
            let mut children = vec![token_leaf(b, condense_tok)];
            let target_start = skip_trivia(tokens, idx + 1);
            let no_gaps_parts: &[&str] = &["no", "gaps"];
            let no_gaps_idx = find_top_level_hyphenated_keyword_index(
                source,
                tokens,
                target_start,
                period_i,
                no_gaps_parts,
            );
            let expr_end = no_gaps_idx.unwrap_or(period_i);

            if target_start >= expr_end {
                errors.push(crate::ParseError {
                    message: "syntax error: expected target variable or expression after CONDENSE"
                        .to_string(),
                    range: condense_tok.range.start..tokens[period_i].range.end,
                });
            } else {
                push_expr_child(
                    b,
                    &mut children,
                    source,
                    tokens,
                    target_start,
                    expr_end,
                    Some(condense_tok),
                );
            }

            if let Some(ng) = no_gaps_idx
                && let Some(ng_end) = match_hyphenated_keyword(source, tokens, ng, no_gaps_parts)
            {
                push_token_children(b, &mut children, tokens, ng, ng_end);
            }

            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::CondenseStmt,
                condense_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        },
    ))
}

pub fn try_parse_raise_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let first = tokens.get(idx)?;
    if !is_keyword(source, first, "raise") {
        return None;
    }

    fn raise_exception_type_prefix_end(
        source: &str,
        tokens: &[Token],
        idx: usize,
    ) -> Option<usize> {
        let mut cursor = idx + 1;
        if tokens
            .get(cursor)
            .is_some_and(|token| is_keyword(source, token, "resumable"))
        {
            cursor += 1;
        }
        if !tokens
            .get(cursor)
            .is_some_and(|token| is_keyword(source, token, "exception"))
            || !tokens
                .get(cursor + 1)
                .is_some_and(|token| is_keyword(source, token, "type"))
        {
            return None;
        }
        Some(cursor + 2)
    }

    fn raise_event_prefix_end(source: &str, tokens: &[Token], idx: usize) -> Option<usize> {
        tokens
            .get(idx + 1)
            .filter(|token| is_keyword(source, token, "event"))
            .map(|_| idx + 2)
    }

    Some(parse_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        idx + 1,
        first,
        "syntax error: expected '.' after RAISE statement",
        errors,
        next_after_unterminated_scan,
        |b, period_i, _errors| {
            if let Some(target_start) = raise_event_prefix_end(source, tokens, idx) {
                let arg_start = scan_until_clause(tokens, target_start, period_i, |tokens, at| {
                    named_argument_section_keyword(source, tokens, at)
                });
                let mut children = Vec::with_capacity(period_i - idx + 1);
                for token in &tokens[idx..arg_start] {
                    children.push(token_leaf(b, token));
                }
                if arg_start < period_i {
                    if let Some(arg_list) =
                        build_call_argument_list_node(b, source, tokens, arg_start, period_i)
                    {
                        children.push(arg_list);
                    } else {
                        for token in &tokens[arg_start..period_i] {
                            children.push(token_leaf(b, token));
                        }
                    }
                }
                children.push(token_leaf(b, &tokens[period_i]));
                let node = b.branch(
                    SyntaxKind::RaiseEventStmt,
                    first.range.start..tokens[period_i].range.end,
                    &children,
                );
                return (node, period_i + 1);
            }

            let Some(type_start) = raise_exception_type_prefix_end(source, tokens, idx) else {
                let children = token_children(b, tokens, idx, period_i + 1);
                let node = b.branch(
                    SyntaxKind::RaiseStmt,
                    first.range.start..tokens[period_i].range.end,
                    &children,
                );
                return (node, period_i + 1);
            };

            let clause_starts = |tokens: &[Token], at: usize| {
                named_argument_section_keyword(source, tokens, at)
                    || tokens.get(at).is_some_and(|token| {
                        is_keyword(source, token, "message") || is_keyword(source, token, "using")
                    })
            };
            let type_end = scan_until_clause(tokens, type_start, period_i, clause_starts);
            if type_start >= type_end {
                let children = token_children(b, tokens, idx, period_i + 1);
                let node = b.branch(
                    SyntaxKind::RaiseStmt,
                    first.range.start..tokens[period_i].range.end,
                    &children,
                );
                return (node, period_i + 1);
            }

            let mut children = Vec::with_capacity(period_i - idx + 1);
            for token in &tokens[idx..type_start] {
                children.push(token_leaf(b, token));
            }
            children.push(build_type_ref_node(
                b,
                source,
                &tokens[type_start..type_end],
            ));
            let arg_start = scan_until_clause(tokens, type_end, period_i, |tokens, at| {
                named_argument_section_keyword(source, tokens, at)
            });
            for token in &tokens[type_end..arg_start] {
                children.push(token_leaf(b, token));
            }
            if arg_start < period_i {
                if let Some(arg_list) =
                    build_call_argument_list_node(b, source, tokens, arg_start, period_i)
                {
                    children.push(arg_list);
                } else {
                    for token in &tokens[arg_start..period_i] {
                        children.push(token_leaf(b, token));
                    }
                }
            }
            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::RaiseStmt,
                first.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        },
    ))
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum MessageClauseKind {
    With,
    Into,
    DisplayLike,
    Raising,
}

fn message_clause_start_kind(
    source: &str,
    tokens: &[Token],
    idx: usize,
) -> Option<MessageClauseKind> {
    let token = tokens.get(idx)?;
    if is_keyword(source, token, "with") {
        return Some(MessageClauseKind::With);
    }
    if is_keyword(source, token, "into") {
        return Some(MessageClauseKind::Into);
    }
    if is_keyword(source, token, "raising") {
        return Some(MessageClauseKind::Raising);
    }
    if is_keyword(source, token, "display")
        && tokens
            .get(idx + 1)
            .is_some_and(|next| is_keyword(source, next, "like"))
    {
        return Some(MessageClauseKind::DisplayLike);
    }
    None
}

fn message_token_is_literal_like(source: &str, token: &Token) -> bool {
    token
        .lexeme(source)
        .chars()
        .next()
        .is_some_and(|ch| ch.is_ascii_digit() || matches!(ch, '\'' | '`' | '|'))
}

fn message_token_starts_operand(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    if !(token.kind == TokenKind::Ident
        || matches!(
            token.kind,
            TokenKind::LParen
                | TokenKind::LBracket
                | TokenKind::LBrace
                | TokenKind::At
                | TokenKind::Hash
                | TokenKind::Number
                | TokenKind::String
                | TokenKind::StringTemplate
        )
        || message_token_is_literal_like(source, token))
    {
        return false;
    }
    let Some(prev) = idx.checked_sub(1).and_then(|prev_idx| tokens.get(prev_idx)) else {
        return true;
    };
    if !have_space_between(prev, token)
        && (matches!(
            token.kind,
            TokenKind::LParen | TokenKind::LBracket | TokenKind::LBrace
        ) || prev.kind == TokenKind::Minus)
    {
        return false;
    }
    !(prev.kind == TokenKind::Ident
        && matches!(
            prev.lexeme(source).to_ascii_lowercase().as_str(),
            "new" | "ref" | "to"
        ))
}

fn consume_message_operand(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
    clause_keywords: &[&str],
) -> usize {
    let mut idx = start;
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut consumed_any = false;

    while idx < end_exclusive {
        let token = &tokens[idx];
        if paren == 0 && bracket == 0 && brace == 0 {
            if token.kind == TokenKind::Period {
                break;
            }
            if token.kind == TokenKind::Ident
                && clause_keywords
                    .iter()
                    .any(|keyword| token.lexeme(source).eq_ignore_ascii_case(keyword))
            {
                break;
            }
            if consumed_any && message_token_starts_operand(source, tokens, idx) {
                break;
            }
        }

        consumed_any = true;
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

fn message_text_pool_operand_end(source: &str, tokens: &[Token], start: usize) -> Option<usize> {
    let head = tokens.get(start)?;
    let dash = tokens.get(start + 1)?;
    let number = tokens.get(start + 2)?;
    (head.kind == TokenKind::Ident
        && head.lexeme(source).eq_ignore_ascii_case("text")
        && dash.kind == TokenKind::Minus
        && number.lexeme(source).chars().all(|ch| ch.is_ascii_digit()))
    .then_some(start + 3)
}

fn message_compact_code_tokens(source: &str, tokens: &[Token], start: usize, end: usize) -> bool {
    let Some(head) = tokens.get(start) else {
        return false;
    };
    let mut chars = head.lexeme(source).chars();
    let Some(msgty) = chars.next() else {
        return false;
    };
    if !matches!(
        msgty.to_ascii_lowercase(),
        'a' | 'e' | 'i' | 's' | 'w' | 'x'
    ) {
        return false;
    }
    chars.all(|ch| ch.is_ascii_digit())
        && tokens.get(start + 1).map(|token| token.kind) == Some(TokenKind::LParen)
        && tokens.get(end.saturating_sub(1)).map(|token| token.kind) == Some(TokenKind::RParen)
}

fn push_message_operand_node(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    operand_kind: SyntaxKind,
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
    prev_before_first: Option<&Token>,
) {
    if start >= end_exclusive {
        return;
    }
    let inline_end = trim_trailing_comment_tokens(tokens, start, end_exclusive);
    let mut operand_children = Vec::new();

    if let Some(text_pool_end) = message_text_pool_operand_end(source, tokens, start)
        && text_pool_end == inline_end
    {
        let text_pool_children = token_children(b, tokens, start, text_pool_end);
        operand_children.push(b.branch(
            SyntaxKind::MessageTextPoolId,
            tokens[start].range.start..tokens[text_pool_end - 1].range.end,
            &text_pool_children,
        ));
    } else if operand_kind == SyntaxKind::MessageCodeOperand
        && message_compact_code_tokens(source, tokens, start, inline_end)
    {
        operand_children.extend(token_children(b, tokens, start, end_exclusive));
    } else {
        push_call_argument_value_child(
            b,
            &mut operand_children,
            source,
            tokens,
            start,
            inline_end,
            prev_before_first,
        );
        push_token_children(b, &mut operand_children, tokens, inline_end, end_exclusive);
    }

    children.push(b.branch(
        operand_kind,
        tokens[start].range.start..tokens[end_exclusive - 1].range.end,
        &operand_children,
    ));
}

fn build_message_head_clause_node(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> Option<NodeId> {
    if start >= end_exclusive {
        return None;
    }

    let mut children = Vec::new();
    let mut cursor = start;
    if tokens
        .get(cursor)
        .is_some_and(|token| is_keyword(source, token, "id"))
    {
        let id_tok = &tokens[cursor];
        children.push(token_leaf(b, id_tok));
        cursor += 1;
        let id_end =
            consume_message_operand(source, tokens, cursor, end_exclusive, &["type", "number"]);
        push_message_operand_node(
            b,
            &mut children,
            SyntaxKind::MessageIdOperand,
            source,
            tokens,
            cursor,
            id_end,
            Some(id_tok),
        );
        cursor = id_end;
        if cursor < end_exclusive
            && tokens
                .get(cursor)
                .is_some_and(|token| is_keyword(source, token, "type"))
        {
            let type_tok = &tokens[cursor];
            children.push(token_leaf(b, type_tok));
            cursor += 1;
            let type_end =
                consume_message_operand(source, tokens, cursor, end_exclusive, &["number"]);
            push_message_operand_node(
                b,
                &mut children,
                SyntaxKind::MessageTypeOperand,
                source,
                tokens,
                cursor,
                type_end,
                Some(type_tok),
            );
            cursor = type_end;
        }
        if cursor < end_exclusive
            && tokens
                .get(cursor)
                .is_some_and(|token| is_keyword(source, token, "number"))
        {
            let number_tok = &tokens[cursor];
            children.push(token_leaf(b, number_tok));
            cursor += 1;
            push_message_operand_node(
                b,
                &mut children,
                SyntaxKind::MessageNumberOperand,
                source,
                tokens,
                cursor,
                end_exclusive,
                Some(number_tok),
            );
        }
    } else {
        let code_end = consume_message_operand(source, tokens, cursor, end_exclusive, &["type"]);
        push_message_operand_node(
            b,
            &mut children,
            SyntaxKind::MessageCodeOperand,
            source,
            tokens,
            cursor,
            code_end,
            None,
        );
        cursor = code_end;
        if cursor < end_exclusive
            && tokens
                .get(cursor)
                .is_some_and(|token| is_keyword(source, token, "type"))
        {
            let type_tok = &tokens[cursor];
            children.push(token_leaf(b, type_tok));
            cursor += 1;
            push_message_operand_node(
                b,
                &mut children,
                SyntaxKind::MessageTypeOperand,
                source,
                tokens,
                cursor,
                end_exclusive,
                Some(type_tok),
            );
        }
    }

    Some(b.branch(
        SyntaxKind::MessageHeadClause,
        tokens[start].range.start..tokens[end_exclusive - 1].range.end,
        &children,
    ))
}

fn build_message_with_clause_node(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> Option<NodeId> {
    if start >= end_exclusive {
        return None;
    }
    let with_tok = tokens.get(start)?;
    let mut children = vec![token_leaf(b, with_tok)];
    let mut cursor = start + 1;
    while cursor < end_exclusive {
        let operand_end = consume_message_operand(source, tokens, cursor, end_exclusive, &[]);
        if operand_end <= cursor {
            break;
        }
        if let Some(text_pool_end) = message_text_pool_operand_end(source, tokens, cursor)
            && text_pool_end == trim_trailing_comment_tokens(tokens, cursor, operand_end)
        {
            let text_pool_children = token_children(b, tokens, cursor, text_pool_end);
            children.push(b.branch(
                SyntaxKind::MessageTextPoolId,
                tokens[cursor].range.start..tokens[text_pool_end - 1].range.end,
                &text_pool_children,
            ));
            push_token_children(b, &mut children, tokens, text_pool_end, operand_end);
        } else {
            push_message_operand_node(
                b,
                &mut children,
                SyntaxKind::MessageOperand,
                source,
                tokens,
                cursor,
                operand_end,
                tokens.get(cursor.saturating_sub(1)),
            );
        }
        cursor = operand_end;
    }
    Some(b.branch(
        SyntaxKind::MessageWithClause,
        tokens[start].range.start..tokens[end_exclusive - 1].range.end,
        &children,
    ))
}

pub fn try_parse_message_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let message_tok = tokens.get(idx)?;
    if !is_keyword(source, message_tok, "message") {
        return None;
    }

    Some(parse_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        idx + 1,
        message_tok,
        "syntax error: expected '.' after MESSAGE statement",
        errors,
        next_after_unterminated_scan,
        |b, period_i, _errors| {
            let mut children = vec![token_leaf(b, message_tok)];
            let clause_starts = |tokens: &[Token], at: usize| {
                message_clause_start_kind(source, tokens, at).is_some()
            };

            let mut cursor = idx + 1;
            let head_end = scan_until_clause(tokens, cursor, period_i, clause_starts);
            if cursor < head_end {
                if let Some(head_clause) =
                    build_message_head_clause_node(b, source, tokens, cursor, head_end)
                {
                    children.push(head_clause);
                }
            }
            cursor = head_end;

            while cursor < period_i {
                match message_clause_start_kind(source, tokens, cursor) {
                    Some(MessageClauseKind::With) => {
                        let end = scan_until_clause(tokens, cursor + 1, period_i, clause_starts);
                        if let Some(with_clause) =
                            build_message_with_clause_node(b, source, tokens, cursor, end)
                        {
                            children.push(with_clause);
                        }
                        cursor = end;
                    }
                    Some(MessageClauseKind::Into) => {
                        let end = scan_until_clause(tokens, cursor + 1, period_i, clause_starts);
                        let mut clause_children = vec![token_leaf(b, &tokens[cursor])];
                        let target_start = skip_trivia(tokens, cursor + 1);
                        push_token_children(
                            b,
                            &mut clause_children,
                            tokens,
                            cursor + 1,
                            target_start,
                        );
                        let target_end = trim_trailing_comment_tokens(tokens, target_start, end);
                        push_call_argument_value_child(
                            b,
                            &mut clause_children,
                            source,
                            tokens,
                            target_start,
                            target_end,
                            Some(&tokens[cursor]),
                        );
                        push_token_children(b, &mut clause_children, tokens, target_end, end);
                        children.push(b.branch(
                            SyntaxKind::MessageIntoClause,
                            tokens[cursor].range.start..tokens[end - 1].range.end,
                            &clause_children,
                        ));
                        cursor = end;
                    }
                    Some(MessageClauseKind::DisplayLike) => {
                        let end = scan_until_clause(tokens, cursor + 2, period_i, clause_starts);
                        let mut clause_children = vec![
                            token_leaf(b, &tokens[cursor]),
                            token_leaf(b, &tokens[cursor + 1]),
                        ];
                        let expr_start = skip_trivia(tokens, cursor + 2);
                        push_token_children(
                            b,
                            &mut clause_children,
                            tokens,
                            cursor + 2,
                            expr_start,
                        );
                        push_expr_child(
                            b,
                            &mut clause_children,
                            source,
                            tokens,
                            expr_start,
                            end,
                            Some(&tokens[cursor + 1]),
                        );
                        children.push(b.branch(
                            SyntaxKind::MessageDisplayLikeClause,
                            tokens[cursor].range.start..tokens[end - 1].range.end,
                            &clause_children,
                        ));
                        cursor = end;
                    }
                    Some(MessageClauseKind::Raising) => {
                        let end = scan_until_clause(tokens, cursor + 1, period_i, clause_starts);
                        let mut clause_children = vec![token_leaf(b, &tokens[cursor])];
                        let expr_start = skip_trivia(tokens, cursor + 1);
                        push_token_children(
                            b,
                            &mut clause_children,
                            tokens,
                            cursor + 1,
                            expr_start,
                        );
                        push_expr_child(
                            b,
                            &mut clause_children,
                            source,
                            tokens,
                            expr_start,
                            end,
                            Some(&tokens[cursor]),
                        );
                        children.push(b.branch(
                            SyntaxKind::MessageRaisingClause,
                            tokens[cursor].range.start..tokens[end - 1].range.end,
                            &clause_children,
                        ));
                        cursor = end;
                    }
                    None => {
                        let clause_children = token_children(b, tokens, cursor, period_i);
                        children.push(b.branch(
                            SyntaxKind::MessageHeadClause,
                            tokens[cursor].range.start..tokens[period_i - 1].range.end,
                            &clause_children,
                        ));
                        cursor = period_i;
                    }
                }
            }

            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::MessageStmt,
                message_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        },
    ))
}

pub fn try_parse_submit_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let submit_tok = tokens.get(idx)?;
    if !is_keyword(source, submit_tok, "submit") {
        return None;
    }
    Some(parse_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        idx + 1,
        submit_tok,
        "syntax error: expected '.' after SUBMIT",
        errors,
        next_after_unterminated_scan,
        |b, period_i, _errors| {
            let mut children = vec![token_leaf(b, submit_tok)];
            let mut cursor = idx + 1;

            let target_end = scan_until_clause(tokens, cursor, period_i, |tokens, i| {
                submit_clause_starts(source, tokens, i)
            });
            if target_end > cursor {
                let mut target_children = Vec::new();
                if tokens[cursor].kind == TokenKind::LParen {
                    if let Some(rparen_idx) =
                        find_matching_delim(tokens, cursor, TokenKind::LParen, TokenKind::RParen)
                        && rparen_idx < target_end
                    {
                        push_token_children(b, &mut target_children, tokens, cursor, cursor + 1);
                        push_expr_child(
                            b,
                            &mut target_children,
                            source,
                            tokens,
                            cursor + 1,
                            rparen_idx,
                            Some(&tokens[cursor]),
                        );
                        push_token_children(
                            b,
                            &mut target_children,
                            tokens,
                            rparen_idx,
                            rparen_idx + 1,
                        );
                        if rparen_idx + 1 < target_end {
                            push_token_children(
                                b,
                                &mut target_children,
                                tokens,
                                rparen_idx + 1,
                                target_end,
                            );
                        }
                    } else {
                        push_token_children(b, &mut target_children, tokens, cursor, target_end);
                    }
                } else {
                    push_token_children(b, &mut target_children, tokens, cursor, target_end);
                }
                children.push(b.branch(
                    SyntaxKind::SubmitTarget,
                    tokens[cursor].range.start..tokens[target_end - 1].range.end,
                    &target_children,
                ));
                cursor = target_end;
            }

            while cursor < period_i {
                if let Some(lead_end) = match_submit_sequence(
                    source,
                    tokens,
                    cursor,
                    &["using", "selection", "-", "screen"],
                ) {
                    let expr_end =
                        scan_submit_expr_end(source, tokens, lead_end, period_i, |_, _| false);
                    let clause_end = expr_end.max(lead_end);
                    let mut clause_children = Vec::new();
                    push_token_children(b, &mut clause_children, tokens, cursor, lead_end);
                    push_expr_child(
                        b,
                        &mut clause_children,
                        source,
                        tokens,
                        lead_end,
                        expr_end,
                        lead_end.checked_sub(1).and_then(|i| tokens.get(i)),
                    );
                    children.push(b.branch(
                        SyntaxKind::SubmitSelectionScreenOperand,
                        tokens[cursor].range.start..tokens[clause_end - 1].range.end,
                        &clause_children,
                    ));
                    cursor = clause_end;
                    continue;
                }
                if let Some(next) = match_submit_sequence(
                    source,
                    tokens,
                    cursor,
                    &["via", "selection", "-", "screen"],
                ) {
                    push_token_children(b, &mut children, tokens, cursor, next);
                    cursor = next;
                    continue;
                }
                if let Some(lead_end) = match_submit_sequence(
                    source,
                    tokens,
                    cursor,
                    &["using", "selection", "-", "set"],
                ) {
                    let expr_end =
                        scan_submit_expr_end(source, tokens, lead_end, period_i, |_, _| false);
                    let clause_end = expr_end.max(lead_end);
                    let mut clause_children = Vec::new();
                    push_token_children(b, &mut clause_children, tokens, cursor, lead_end);
                    push_expr_child(
                        b,
                        &mut clause_children,
                        source,
                        tokens,
                        lead_end,
                        expr_end,
                        lead_end.checked_sub(1).and_then(|i| tokens.get(i)),
                    );
                    children.push(b.branch(
                        SyntaxKind::SubmitSelectionSetOperand,
                        tokens[cursor].range.start..tokens[clause_end - 1].range.end,
                        &clause_children,
                    ));
                    cursor = clause_end;
                    continue;
                }
                if let Some(lead_end) = match_submit_sequence(
                    source,
                    tokens,
                    cursor,
                    &["using", "selection", "-", "sets", "of", "program"],
                ) {
                    let expr_end =
                        scan_submit_expr_end(source, tokens, lead_end, period_i, |_, _| false);
                    let clause_end = expr_end.max(lead_end);
                    let mut clause_children = Vec::new();
                    push_token_children(b, &mut clause_children, tokens, cursor, lead_end);
                    push_expr_child(
                        b,
                        &mut clause_children,
                        source,
                        tokens,
                        lead_end,
                        expr_end,
                        lead_end.checked_sub(1).and_then(|i| tokens.get(i)),
                    );
                    children.push(b.branch(
                        SyntaxKind::SubmitSelectionSetsProgramOperand,
                        tokens[cursor].range.start..tokens[clause_end - 1].range.end,
                        &clause_children,
                    ));
                    cursor = clause_end;
                    continue;
                }
                if let Some(lead_end) = match_submit_sequence(
                    source,
                    tokens,
                    cursor,
                    &["with", "selection", "-", "table"],
                ) {
                    let expr_end =
                        scan_submit_expr_end(source, tokens, lead_end, period_i, |_, _| false);
                    let clause_end = expr_end.max(lead_end);
                    let mut clause_children = Vec::new();
                    push_token_children(b, &mut clause_children, tokens, cursor, lead_end);
                    push_expr_child(
                        b,
                        &mut clause_children,
                        source,
                        tokens,
                        lead_end,
                        expr_end,
                        lead_end.checked_sub(1).and_then(|i| tokens.get(i)),
                    );
                    children.push(b.branch(
                        SyntaxKind::SubmitSelectionTableOperand,
                        tokens[cursor].range.start..tokens[clause_end - 1].range.end,
                        &clause_children,
                    ));
                    cursor = clause_end;
                    continue;
                }
                if let Some(lead_end) =
                    match_submit_sequence(source, tokens, cursor, &["with", "free", "selections"])
                {
                    let expr_end =
                        scan_submit_expr_end(source, tokens, lead_end, period_i, |_, _| false);
                    let clause_end = expr_end.max(lead_end);
                    let mut clause_children = Vec::new();
                    push_token_children(b, &mut clause_children, tokens, cursor, lead_end);
                    push_expr_child(
                        b,
                        &mut clause_children,
                        source,
                        tokens,
                        lead_end,
                        expr_end,
                        lead_end.checked_sub(1).and_then(|i| tokens.get(i)),
                    );
                    children.push(b.branch(
                        SyntaxKind::SubmitFreeSelectionsOperand,
                        tokens[cursor].range.start..tokens[clause_end - 1].range.end,
                        &clause_children,
                    ));
                    cursor = clause_end;
                    continue;
                }
                if is_keyword(source, &tokens[cursor], "with") && cursor + 1 < period_i {
                    let selector_end = cursor + 2;
                    let operator_idx = selector_end;
                    let mut clause_children = Vec::new();
                    let mut clause_end = selector_end;
                    let mut handled = false;

                    if let Some(op_end) =
                        match_submit_sequence(source, tokens, operator_idx, &["not", "between"])
                    {
                        let low_start = op_end;
                        let low_end =
                            scan_until_clause(tokens, low_start, period_i, |tokens, i| {
                                tokens
                                    .get(i)
                                    .is_some_and(|token| is_keyword(source, token, "and"))
                                    || submit_clause_starts(source, tokens, i)
                            });
                        let high_start = if tokens
                            .get(low_end)
                            .is_some_and(|token| is_keyword(source, token, "and"))
                        {
                            low_end + 1
                        } else {
                            low_end
                        };
                        let high_end = scan_submit_expr_end(
                            source,
                            tokens,
                            high_start,
                            period_i,
                            |tokens, i| {
                                match_submit_sequence(source, tokens, i, &["sign"]).is_some()
                            },
                        );
                        push_token_children(b, &mut clause_children, tokens, cursor, low_start);
                        push_expr_child(
                            b,
                            &mut clause_children,
                            source,
                            tokens,
                            low_start,
                            low_end,
                            low_start.checked_sub(1).and_then(|i| tokens.get(i)),
                        );
                        push_token_children(b, &mut clause_children, tokens, low_end, high_start);
                        push_expr_child(
                            b,
                            &mut clause_children,
                            source,
                            tokens,
                            high_start,
                            high_end,
                            high_start.checked_sub(1).and_then(|i| tokens.get(i)),
                        );
                        clause_end = high_end;
                        if let Some(sign_end) =
                            match_submit_sequence(source, tokens, clause_end, &["sign"])
                        {
                            let value_end =
                                scan_submit_expr_end(source, tokens, sign_end, period_i, |_, _| {
                                    false
                                });
                            push_token_children(
                                b,
                                &mut clause_children,
                                tokens,
                                clause_end,
                                sign_end,
                            );
                            push_expr_child(
                                b,
                                &mut clause_children,
                                source,
                                tokens,
                                sign_end,
                                value_end,
                                sign_end.checked_sub(1).and_then(|i| tokens.get(i)),
                            );
                            clause_end = value_end.max(sign_end);
                        }
                        handled = true;
                    } else if let Some(op_end) =
                        match_submit_sequence(source, tokens, operator_idx, &["between"])
                    {
                        let low_start = op_end;
                        let low_end =
                            scan_until_clause(tokens, low_start, period_i, |tokens, i| {
                                tokens
                                    .get(i)
                                    .is_some_and(|token| is_keyword(source, token, "and"))
                                    || submit_clause_starts(source, tokens, i)
                            });
                        let high_start = if tokens
                            .get(low_end)
                            .is_some_and(|token| is_keyword(source, token, "and"))
                        {
                            low_end + 1
                        } else {
                            low_end
                        };
                        let high_end = scan_submit_expr_end(
                            source,
                            tokens,
                            high_start,
                            period_i,
                            |tokens, i| {
                                match_submit_sequence(source, tokens, i, &["sign"]).is_some()
                            },
                        );
                        push_token_children(b, &mut clause_children, tokens, cursor, low_start);
                        push_expr_child(
                            b,
                            &mut clause_children,
                            source,
                            tokens,
                            low_start,
                            low_end,
                            low_start.checked_sub(1).and_then(|i| tokens.get(i)),
                        );
                        push_token_children(b, &mut clause_children, tokens, low_end, high_start);
                        push_expr_child(
                            b,
                            &mut clause_children,
                            source,
                            tokens,
                            high_start,
                            high_end,
                            high_start.checked_sub(1).and_then(|i| tokens.get(i)),
                        );
                        clause_end = high_end;
                        if let Some(sign_end) =
                            match_submit_sequence(source, tokens, clause_end, &["sign"])
                        {
                            let value_end =
                                scan_submit_expr_end(source, tokens, sign_end, period_i, |_, _| {
                                    false
                                });
                            push_token_children(
                                b,
                                &mut clause_children,
                                tokens,
                                clause_end,
                                sign_end,
                            );
                            push_expr_child(
                                b,
                                &mut clause_children,
                                source,
                                tokens,
                                sign_end,
                                value_end,
                                sign_end.checked_sub(1).and_then(|i| tokens.get(i)),
                            );
                            clause_end = value_end.max(sign_end);
                        }
                        handled = true;
                    } else if let Some(op_end) =
                        match_submit_sequence(source, tokens, operator_idx, &["in"])
                    {
                        let value_end =
                            scan_submit_expr_end(source, tokens, op_end, period_i, |_, _| false);
                        push_token_children(b, &mut clause_children, tokens, cursor, op_end);
                        push_expr_child(
                            b,
                            &mut clause_children,
                            source,
                            tokens,
                            op_end,
                            value_end,
                            op_end.checked_sub(1).and_then(|i| tokens.get(i)),
                        );
                        clause_end = value_end.max(op_end);
                        handled = true;
                    } else if submit_is_comparison_operator(source, tokens, operator_idx) {
                        let value_start = operator_idx + 1;
                        let value_end = scan_submit_expr_end(
                            source,
                            tokens,
                            value_start,
                            period_i,
                            |tokens, i| {
                                match_submit_sequence(source, tokens, i, &["sign"]).is_some()
                            },
                        );
                        push_token_children(b, &mut clause_children, tokens, cursor, value_start);
                        push_expr_child(
                            b,
                            &mut clause_children,
                            source,
                            tokens,
                            value_start,
                            value_end,
                            value_start.checked_sub(1).and_then(|i| tokens.get(i)),
                        );
                        clause_end = value_end.max(value_start);
                        if let Some(sign_end) =
                            match_submit_sequence(source, tokens, clause_end, &["sign"])
                        {
                            let sign_value_end =
                                scan_submit_expr_end(source, tokens, sign_end, period_i, |_, _| {
                                    false
                                });
                            push_token_children(
                                b,
                                &mut clause_children,
                                tokens,
                                clause_end,
                                sign_end,
                            );
                            push_expr_child(
                                b,
                                &mut clause_children,
                                source,
                                tokens,
                                sign_end,
                                sign_value_end,
                                sign_end.checked_sub(1).and_then(|i| tokens.get(i)),
                            );
                            clause_end = sign_value_end.max(sign_end);
                        }
                        handled = true;
                    }

                    if handled {
                        children.push(b.branch(
                            SyntaxKind::SubmitWithClause,
                            tokens[cursor].range.start..tokens[clause_end - 1].range.end,
                            &clause_children,
                        ));
                        cursor = clause_end;
                        continue;
                    }
                }
                if let Some(lead_end) =
                    match_submit_sequence(source, tokens, cursor, &["line", "-", "size"])
                {
                    let expr_end =
                        scan_submit_expr_end(source, tokens, lead_end, period_i, |_, _| false);
                    let clause_end = expr_end.max(lead_end);
                    let mut clause_children = Vec::new();
                    push_token_children(b, &mut clause_children, tokens, cursor, lead_end);
                    push_expr_child(
                        b,
                        &mut clause_children,
                        source,
                        tokens,
                        lead_end,
                        expr_end,
                        lead_end.checked_sub(1).and_then(|i| tokens.get(i)),
                    );
                    children.push(b.branch(
                        SyntaxKind::SubmitLineSizeOperand,
                        tokens[cursor].range.start..tokens[clause_end - 1].range.end,
                        &clause_children,
                    ));
                    cursor = clause_end;
                    continue;
                }
                if let Some(lead_end) =
                    match_submit_sequence(source, tokens, cursor, &["line", "-", "count"])
                {
                    let expr_end =
                        scan_submit_expr_end(source, tokens, lead_end, period_i, |_, _| false);
                    let clause_end = expr_end.max(lead_end);
                    let mut clause_children = Vec::new();
                    push_token_children(b, &mut clause_children, tokens, cursor, lead_end);
                    push_expr_child(
                        b,
                        &mut clause_children,
                        source,
                        tokens,
                        lead_end,
                        expr_end,
                        lead_end.checked_sub(1).and_then(|i| tokens.get(i)),
                    );
                    children.push(b.branch(
                        SyntaxKind::SubmitLineCountOperand,
                        tokens[cursor].range.start..tokens[clause_end - 1].range.end,
                        &clause_children,
                    ));
                    cursor = clause_end;
                    continue;
                }
                if let Some(next) = match_submit_sequence(
                    source,
                    tokens,
                    cursor,
                    &["exporting", "list", "to", "memory"],
                ) {
                    push_token_children(b, &mut children, tokens, cursor, next);
                    cursor = next;
                    continue;
                }
                if let Some(next) =
                    match_submit_sequence(source, tokens, cursor, &["to", "sap", "-", "spool"])
                {
                    push_token_children(b, &mut children, tokens, cursor, next);
                    cursor = next;
                    continue;
                }
                if let Some(lead_end) =
                    match_submit_sequence(source, tokens, cursor, &["spool", "parameters"])
                {
                    let expr_end =
                        scan_submit_expr_end(source, tokens, lead_end, period_i, |tokens, i| {
                            match_submit_sequence(source, tokens, i, &["archive", "parameters"])
                                .is_some()
                                || match_submit_sequence(
                                    source,
                                    tokens,
                                    i,
                                    &["without", "spool", "dynpro"],
                                )
                                .is_some()
                        });
                    let clause_end = expr_end.max(lead_end);
                    let mut clause_children = Vec::new();
                    push_token_children(b, &mut clause_children, tokens, cursor, lead_end);
                    push_expr_child(
                        b,
                        &mut clause_children,
                        source,
                        tokens,
                        lead_end,
                        expr_end,
                        lead_end.checked_sub(1).and_then(|i| tokens.get(i)),
                    );
                    children.push(b.branch(
                        SyntaxKind::SubmitSpoolParametersOperand,
                        tokens[cursor].range.start..tokens[clause_end - 1].range.end,
                        &clause_children,
                    ));
                    cursor = clause_end;
                    continue;
                }
                if let Some(lead_end) =
                    match_submit_sequence(source, tokens, cursor, &["archive", "parameters"])
                {
                    let expr_end =
                        scan_submit_expr_end(source, tokens, lead_end, period_i, |tokens, i| {
                            match_submit_sequence(
                                source,
                                tokens,
                                i,
                                &["without", "spool", "dynpro"],
                            )
                            .is_some()
                        });
                    let clause_end = expr_end.max(lead_end);
                    let mut clause_children = Vec::new();
                    push_token_children(b, &mut clause_children, tokens, cursor, lead_end);
                    push_expr_child(
                        b,
                        &mut clause_children,
                        source,
                        tokens,
                        lead_end,
                        expr_end,
                        lead_end.checked_sub(1).and_then(|i| tokens.get(i)),
                    );
                    children.push(b.branch(
                        SyntaxKind::SubmitArchiveParametersOperand,
                        tokens[cursor].range.start..tokens[clause_end - 1].range.end,
                        &clause_children,
                    ));
                    cursor = clause_end;
                    continue;
                }
                if let Some(next) =
                    match_submit_sequence(source, tokens, cursor, &["without", "spool", "dynpro"])
                {
                    push_token_children(b, &mut children, tokens, cursor, next);
                    cursor = next;
                    continue;
                }
                if let Some(lead_end) = match_submit_sequence(source, tokens, cursor, &["user"]) {
                    let expr_end =
                        scan_submit_expr_end(source, tokens, lead_end, period_i, |_, _| false);
                    let clause_end = expr_end.max(lead_end);
                    let mut clause_children = Vec::new();
                    push_token_children(b, &mut clause_children, tokens, cursor, lead_end);
                    push_expr_child(
                        b,
                        &mut clause_children,
                        source,
                        tokens,
                        lead_end,
                        expr_end,
                        lead_end.checked_sub(1).and_then(|i| tokens.get(i)),
                    );
                    children.push(b.branch(
                        SyntaxKind::SubmitUserOperand,
                        tokens[cursor].range.start..tokens[clause_end - 1].range.end,
                        &clause_children,
                    ));
                    cursor = clause_end;
                    continue;
                }
                if let Some(lead_end) =
                    match_submit_sequence(source, tokens, cursor, &["via", "job"])
                {
                    let expr_end =
                        scan_submit_expr_end(source, tokens, lead_end, period_i, |tokens, i| {
                            match_submit_sequence(source, tokens, i, &["number"]).is_some()
                        });
                    let clause_end = expr_end.max(lead_end);
                    let mut clause_children = Vec::new();
                    push_token_children(b, &mut clause_children, tokens, cursor, lead_end);
                    push_expr_child(
                        b,
                        &mut clause_children,
                        source,
                        tokens,
                        lead_end,
                        expr_end,
                        lead_end.checked_sub(1).and_then(|i| tokens.get(i)),
                    );
                    children.push(b.branch(
                        SyntaxKind::SubmitJobOperand,
                        tokens[cursor].range.start..tokens[clause_end - 1].range.end,
                        &clause_children,
                    ));
                    cursor = clause_end;
                    continue;
                }
                if let Some(lead_end) = match_submit_sequence(source, tokens, cursor, &["number"]) {
                    let expr_end =
                        scan_submit_expr_end(source, tokens, lead_end, period_i, |tokens, i| {
                            match_submit_sequence(source, tokens, i, &["language"]).is_some()
                        });
                    let clause_end = expr_end.max(lead_end);
                    let mut clause_children = Vec::new();
                    push_token_children(b, &mut clause_children, tokens, cursor, lead_end);
                    push_expr_child(
                        b,
                        &mut clause_children,
                        source,
                        tokens,
                        lead_end,
                        expr_end,
                        lead_end.checked_sub(1).and_then(|i| tokens.get(i)),
                    );
                    children.push(b.branch(
                        SyntaxKind::SubmitJobNumberOperand,
                        tokens[cursor].range.start..tokens[clause_end - 1].range.end,
                        &clause_children,
                    ));
                    cursor = clause_end;
                    continue;
                }
                if let Some(lead_end) = match_submit_sequence(source, tokens, cursor, &["language"])
                {
                    let expr_end =
                        scan_submit_expr_end(source, tokens, lead_end, period_i, |_, _| false);
                    let clause_end = expr_end.max(lead_end);
                    let mut clause_children = Vec::new();
                    push_token_children(b, &mut clause_children, tokens, cursor, lead_end);
                    push_expr_child(
                        b,
                        &mut clause_children,
                        source,
                        tokens,
                        lead_end,
                        expr_end,
                        lead_end.checked_sub(1).and_then(|i| tokens.get(i)),
                    );
                    children.push(b.branch(
                        SyntaxKind::SubmitLanguageOperand,
                        tokens[cursor].range.start..tokens[clause_end - 1].range.end,
                        &clause_children,
                    ));
                    cursor = clause_end;
                    continue;
                }
                if let Some(next) =
                    match_submit_sequence(source, tokens, cursor, &["and", "return"])
                {
                    push_token_children(b, &mut children, tokens, cursor, next);
                    cursor = next;
                    continue;
                }

                children.push(token_leaf(b, &tokens[cursor]));
                cursor += 1;
            }

            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::SubmitStmt,
                submit_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        },
    ))
}

pub fn try_parse_leave_stmt(
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
        SyntaxKind::LeaveStmt,
        "leave",
        errors,
        "syntax error: expected '.' after LEAVE statement",
    )
}

pub fn try_parse_selection_screen_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let lead_end = match_hyphenated_keyword(source, tokens, idx, &["selection", "screen"])?;
    let start_tok = tokens.get(idx)?;
    Some(parse_selection_screen_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        lead_end,
        start_tok,
        "syntax error: expected '.' after SELECTION-SCREEN statement",
        errors,
        |b, period_i, _errors| {
            let children = token_children(b, tokens, idx, period_i + 1);
            let node = b.branch(
                SyntaxKind::SelectionScreenStmt,
                start_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        },
    ))
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

pub fn try_parse_find_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let find_tok = tokens.get(idx)?;
    if !is_keyword(source, find_tok, "find") {
        return None;
    }
    Some(parse_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        idx + 1,
        find_tok,
        "syntax error: expected '.' after FIND",
        errors,
        next_after_unterminated_scan,
        |b, period_i, _errors| {
            let mut children = vec![token_leaf(b, find_tok)];
            let mut i = idx + 1;
            if tokens.get(i).is_some_and(|token| {
                is_keyword(source, token, "first") || is_keyword(source, token, "all")
            }) {
                children.push(token_leaf(b, &tokens[i]));
                i += 1;
                if tokens.get(i).is_some_and(|token| {
                    is_keyword(source, token, "occurrence")
                        || is_keyword(source, token, "occurrences")
                }) {
                    children.push(token_leaf(b, &tokens[i]));
                    i += 1;
                }
            }
            if tokens
                .get(i)
                .is_some_and(|token| is_keyword(source, token, "of"))
            {
                children.push(token_leaf(b, &tokens[i]));
                i += 1;
            }
            if tokens
                .get(i)
                .is_some_and(|token| is_keyword(source, token, "regex"))
            {
                children.push(token_leaf(b, &tokens[i]));
                i += 1;
            }

            let Some(in_idx) = find_top_level_keyword_index(source, tokens, i, period_i, "in")
            else {
                let raw = token_children(b, tokens, idx, period_i + 1);
                let node = b.branch(
                    SyntaxKind::Error,
                    find_tok.range.start..tokens[period_i].range.end,
                    &raw,
                );
                return (node, period_i + 1);
            };
            push_wrapped_expr_child(
                b,
                &mut children,
                source,
                tokens,
                i,
                in_idx,
                Some(if i == idx + 1 {
                    find_tok
                } else {
                    &tokens[i - 1]
                }),
                SyntaxKind::FindPatternOperand,
            );
            children.push(token_leaf(b, &tokens[in_idx]));

            let clause_starts = [
                "match",
                "submatches",
                "results",
                "ignoring",
                "respecting",
                "in",
            ];
            let target_end =
                consume_concatenate_operand(source, tokens, in_idx + 1, period_i, &clause_starts);
            push_wrapped_expr_child(
                b,
                &mut children,
                source,
                tokens,
                in_idx + 1,
                target_end,
                Some(&tokens[in_idx]),
                SyntaxKind::FindInOperand,
            );
            i = target_end;

            while i < period_i {
                let token = &tokens[i];
                if is_keyword(source, token, "match") {
                    children.push(token_leaf(b, token));
                    let mut value_start = i + 1;
                    if tokens.get(value_start).is_some_and(|next| {
                        is_keyword(source, next, "offset") || is_keyword(source, next, "length")
                    }) {
                        children.push(token_leaf(b, &tokens[value_start]));
                        value_start += 1;
                    }
                    if let Some(next_idx) = push_wrapped_data_inline_decl_child(
                        b,
                        &mut children,
                        source,
                        tokens,
                        value_start,
                        SyntaxKind::FindMatchTarget,
                    ) {
                        i = next_idx;
                        continue;
                    }
                    let end_idx = consume_concatenate_operand(
                        source,
                        tokens,
                        value_start,
                        period_i,
                        &clause_starts,
                    );
                    push_wrapped_expr_child(
                        b,
                        &mut children,
                        source,
                        tokens,
                        value_start,
                        end_idx,
                        Some(if value_start == i + 1 {
                            token
                        } else {
                            &tokens[value_start - 1]
                        }),
                        SyntaxKind::FindMatchTarget,
                    );
                    i = end_idx;
                    continue;
                }
                if is_keyword(source, token, "submatches") {
                    children.push(token_leaf(b, token));
                    i += 1;
                    while i < period_i {
                        if clause_starts
                            .iter()
                            .any(|keyword| is_keyword(source, &tokens[i], keyword))
                        {
                            break;
                        }
                        if let Some(next_idx) = push_wrapped_data_inline_decl_child(
                            b,
                            &mut children,
                            source,
                            tokens,
                            i,
                            SyntaxKind::FindSubmatchTarget,
                        ) {
                            i = next_idx;
                            continue;
                        }
                        let end_idx = consume_concatenate_operand(
                            source,
                            tokens,
                            i,
                            period_i,
                            &clause_starts,
                        );
                        if end_idx == i {
                            children.push(token_leaf(b, &tokens[i]));
                            i += 1;
                            continue;
                        }
                        push_wrapped_expr_child(
                            b,
                            &mut children,
                            source,
                            tokens,
                            i,
                            end_idx,
                            Some(if i == idx + 1 {
                                find_tok
                            } else {
                                &tokens[i - 1]
                            }),
                            SyntaxKind::FindSubmatchTarget,
                        );
                        i = end_idx;
                    }
                    continue;
                }
                if is_keyword(source, token, "results") {
                    children.push(token_leaf(b, token));
                    let target_start = skip_trivia(tokens, i + 1);
                    if let Some(next_idx) = push_wrapped_data_inline_decl_child(
                        b,
                        &mut children,
                        source,
                        tokens,
                        target_start,
                        SyntaxKind::FindResultsTarget,
                    ) {
                        i = next_idx;
                        continue;
                    }
                    let end_idx = consume_concatenate_operand(
                        source,
                        tokens,
                        target_start,
                        period_i,
                        &clause_starts,
                    );
                    push_wrapped_expr_child(
                        b,
                        &mut children,
                        source,
                        tokens,
                        target_start,
                        end_idx,
                        Some(token),
                        SyntaxKind::FindResultsTarget,
                    );
                    i = end_idx;
                    continue;
                }
                children.push(token_leaf(b, token));
                i += 1;
            }

            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::FindStmt,
                find_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        },
    ))
}

pub fn try_parse_get_bit_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let get_tok = tokens.get(idx)?;
    let lead_end = match_keyword_sequence(source, tokens, idx, &["get", "bit"])?;
    Some(parse_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        lead_end,
        get_tok,
        "syntax error: expected '.' after GET BIT statement",
        errors,
        |_, end_exclusive| end_exclusive,
        |b, period_i, _errors| {
            let clause_of =
                |t: &[Token], i: usize| t.get(i).is_some_and(|tok| is_keyword(source, tok, "of"));
            let clause_into =
                |t: &[Token], i: usize| t.get(i).is_some_and(|tok| is_keyword(source, tok, "into"));

            let expr_start = skip_trivia(tokens, lead_end);
            let Some(of_idx) =
                find_top_level_keyword_index(source, tokens, expr_start, period_i, "of")
            else {
                let children = token_children(b, tokens, idx, period_i + 1);
                let node = b.branch(
                    SyntaxKind::Error,
                    get_tok.range.start..tokens[period_i].range.end,
                    &children,
                );
                return (node, period_i + 1);
            };

            let mut children = Vec::with_capacity(period_i - idx + 1);
            push_token_children(b, &mut children, tokens, idx, lead_end);
            let _ = scan_and_push_expr_clause(
                b,
                &mut children,
                source,
                tokens,
                expr_start,
                of_idx,
                tokens.get(lead_end.saturating_sub(1)),
                &clause_of,
            );
            children.push(token_leaf(b, &tokens[of_idx]));

            let into_expr_start = skip_trivia(tokens, of_idx + 1);
            let Some(into_idx) =
                find_top_level_keyword_index(source, tokens, into_expr_start, period_i, "into")
            else {
                let children = token_children(b, tokens, idx, period_i + 1);
                let node = b.branch(
                    SyntaxKind::Error,
                    get_tok.range.start..tokens[period_i].range.end,
                    &children,
                );
                return (node, period_i + 1);
            };

            let _ = scan_and_push_expr_clause(
                b,
                &mut children,
                source,
                tokens,
                into_expr_start,
                into_idx,
                Some(&tokens[of_idx]),
                &clause_into,
            );
            children.push(token_leaf(b, &tokens[into_idx]));

            let target_start = skip_trivia(tokens, into_idx + 1);
            if let Some((inline_decl, next_idx)) =
                try_parse_data_inline_decl(b, source, tokens, target_start)
                && skip_trivia(tokens, next_idx) == period_i
            {
                children.push(inline_decl);
            } else {
                push_expr_child(
                    b,
                    &mut children,
                    source,
                    tokens,
                    target_start,
                    period_i,
                    Some(&tokens[into_idx]),
                );
            }

            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::GetBitStmt,
                get_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        },
    ))
}

pub fn try_parse_set_bit_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let set_tok = tokens.get(idx)?;
    let lead_end = match_keyword_sequence(source, tokens, idx, &["set", "bit"])?;
    Some(parse_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        lead_end,
        set_tok,
        "syntax error: expected '.' after SET BIT statement",
        errors,
        |_, end_exclusive| end_exclusive,
        |b, period_i, _errors| {
            let clause_of =
                |t: &[Token], i: usize| t.get(i).is_some_and(|tok| is_keyword(source, tok, "of"));
            let clause_to =
                |t: &[Token], i: usize| t.get(i).is_some_and(|tok| is_keyword(source, tok, "to"));

            let expr_start = skip_trivia(tokens, lead_end);
            let Some(of_idx) =
                find_top_level_keyword_index(source, tokens, expr_start, period_i, "of")
            else {
                let children = token_children(b, tokens, idx, period_i + 1);
                let node = b.branch(
                    SyntaxKind::Error,
                    set_tok.range.start..tokens[period_i].range.end,
                    &children,
                );
                return (node, period_i + 1);
            };

            let mut children = Vec::with_capacity(period_i - idx + 1);
            push_token_children(b, &mut children, tokens, idx, lead_end);
            let _ = scan_and_push_expr_clause(
                b,
                &mut children,
                source,
                tokens,
                expr_start,
                of_idx,
                tokens.get(lead_end.saturating_sub(1)),
                &clause_of,
            );
            children.push(token_leaf(b, &tokens[of_idx]));

            let of_target_start = skip_trivia(tokens, of_idx + 1);
            let Some(to_idx) =
                find_top_level_keyword_index(source, tokens, of_target_start, period_i, "to")
            else {
                let children = token_children(b, tokens, idx, period_i + 1);
                let node = b.branch(
                    SyntaxKind::Error,
                    set_tok.range.start..tokens[period_i].range.end,
                    &children,
                );
                return (node, period_i + 1);
            };

            let _ = scan_and_push_expr_clause(
                b,
                &mut children,
                source,
                tokens,
                of_target_start,
                to_idx,
                Some(&tokens[of_idx]),
                &clause_to,
            );
            children.push(token_leaf(b, &tokens[to_idx]));

            let to_value_start = skip_trivia(tokens, to_idx + 1);
            push_expr_child(
                b,
                &mut children,
                source,
                tokens,
                to_value_start,
                period_i,
                Some(&tokens[to_idx]),
            );
            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::SetBitStmt,
                set_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        },
    ))
}

pub fn try_parse_get_time_stamp_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let get_tok = tokens.get(idx)?;
    let lead_end = match_keyword_sequence(source, tokens, idx, GET_TIME_STAMP_FIELD_LEAD)?;

    Some(parse_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        lead_end,
        get_tok,
        "syntax error: expected '.' after GET TIME STAMP FIELD statement",
        errors,
        |_, end_exclusive| end_exclusive,
        |b, period_i, errors| {
            let mut children = Vec::with_capacity(period_i - idx + 1);
            for token in &tokens[idx..lead_end] {
                children.push(token_leaf(b, token));
            }

            let target_start = skip_trivia(tokens, lead_end);
            if target_start >= period_i {
                errors.push(crate::ParseError {
                    message: "syntax error: expected target after GET TIME STAMP FIELD".to_string(),
                    range: get_tok.range.start..tokens[period_i].range.end,
                });
                let mut error_children = children;
                error_children.push(token_leaf(b, &tokens[period_i]));
                let node = b.branch(
                    SyntaxKind::Error,
                    get_tok.range.start..tokens[period_i].range.end,
                    &error_children,
                );
                return (node, period_i + 1);
            }

            if let Some((inline_decl, next_idx)) =
                try_parse_data_inline_decl(b, source, tokens, target_start)
                && skip_trivia(tokens, next_idx) == period_i
            {
                children.push(inline_decl);
            } else {
                push_expr_child(
                    b,
                    &mut children,
                    source,
                    tokens,
                    target_start,
                    period_i,
                    Some(&tokens[lead_end - 1]),
                );
            }

            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::GetTimeStampStmt,
                get_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        },
    ))
}

pub fn try_parse_get_reference_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let get_tok = tokens.get(idx)?;
    let lead_end = match_keyword_sequence(source, tokens, idx, GET_REFERENCE_OF_LEAD)?;

    Some(parse_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        lead_end,
        get_tok,
        "syntax error: expected '.' after GET REFERENCE OF statement",
        errors,
        |_, end_exclusive| end_exclusive,
        |b, period_i, _errors| {
            let clause_into =
                |t: &[Token], i: usize| t.get(i).is_some_and(|tok| is_keyword(source, tok, "into"));

            let source_start = skip_trivia(tokens, lead_end);
            let Some(into_idx) =
                find_top_level_keyword_index(source, tokens, source_start, period_i, "into")
            else {
                let children = token_children(b, tokens, idx, period_i + 1);
                let node = b.branch(
                    SyntaxKind::Error,
                    get_tok.range.start..tokens[period_i].range.end,
                    &children,
                );
                return (node, period_i + 1);
            };

            let mut children = Vec::with_capacity(period_i - idx + 1);
            push_token_children(b, &mut children, tokens, idx, lead_end);
            let _ = scan_and_push_expr_clause(
                b,
                &mut children,
                source,
                tokens,
                source_start,
                into_idx,
                tokens.get(lead_end.saturating_sub(1)),
                &clause_into,
            );
            children.push(token_leaf(b, &tokens[into_idx]));

            let target_start = skip_trivia(tokens, into_idx + 1);
            if let Some((inline_decl, next_idx)) =
                try_parse_data_inline_decl(b, source, tokens, target_start)
                && skip_trivia(tokens, next_idx) == period_i
            {
                children.push(inline_decl);
            } else {
                push_expr_child(
                    b,
                    &mut children,
                    source,
                    tokens,
                    target_start,
                    period_i,
                    Some(&tokens[into_idx]),
                );
            }

            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::GetReferenceStmt,
                get_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        },
    ))
}

pub fn try_parse_call_like_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let first = tokens.get(idx)?;
    let (lead_kind, lead_end) = call_like_lead_kind(source, tokens, idx)?;

    if let Some(period_i) = scan_until_top_level_period(tokens, lead_end) {
        if lead_kind == CallLikeLeadKind::CallMethod
            && !validate_call_method_inline_args_spacing(source, tokens, idx, period_i)
        {
            errors.push(crate::ParseError {
                message: "syntax error: method call arguments must have whitespace or a line break immediately inside parentheses"
                    .to_string(),
                range: first.range.start..tokens[period_i].range.end,
            });
            let mut children = Vec::with_capacity(period_i - idx + 1);
            for t in &tokens[idx..=period_i] {
                children.push(token_leaf(b, t));
            }
            let node = b.branch(
                SyntaxKind::Error,
                first.range.start..tokens[period_i].range.end,
                &children,
            );
            return Some((node, period_i + 1));
        }
        let kind = match lead_kind {
            CallLikeLeadKind::CreateObject => SyntaxKind::CreateObjectStmt,
            CallLikeLeadKind::CreateData => SyntaxKind::CreateDataStmt,
            CallLikeLeadKind::CallMethod => SyntaxKind::CallMethodStmt,
            CallLikeLeadKind::CallStmt | CallLikeLeadKind::SystemFunctionCall => {
                SyntaxKind::CallStmt
            }
        };
        let mut children = Vec::with_capacity(period_i - idx + 1);
        match lead_kind {
            CallLikeLeadKind::CallMethod => {
                children.push(token_leaf(b, &tokens[idx]));
                children.push(token_leaf(b, &tokens[idx + 1]));
                let callee_end = scan_until_clause(tokens, lead_end, period_i, |tokens, at| {
                    call_method_clause_starts(source, tokens, at)
                });
                let callee = parse_arithmetic_expr(b, source, &tokens[lead_end..callee_end], None);
                children.push(b.branch(
                    SyntaxKind::CallMethodTarget,
                    tokens[lead_end].range.start..tokens[callee_end - 1].range.end,
                    &[callee],
                ));
                if let Some(arg_list) =
                    build_call_argument_list_node(b, source, tokens, callee_end, period_i)
                {
                    children.push(arg_list);
                }
                children.push(token_leaf(b, &tokens[period_i]));
            }
            CallLikeLeadKind::CreateObject => {
                children.push(token_leaf(b, &tokens[idx]));
                children.push(token_leaf(b, &tokens[idx + 1]));
                let clause_starts = |tokens: &[Token], at: usize| {
                    tokens.get(at).is_some_and(|token| {
                        is_keyword(source, token, "type")
                            || named_argument_section_keyword(source, tokens, at)
                    })
                };
                let mut cursor = lead_end;
                let target_end = scan_until_clause(tokens, cursor, period_i, clause_starts);
                push_expr_child(b, &mut children, source, tokens, cursor, target_end, None);
                cursor = target_end;
                if cursor < period_i
                    && tokens
                        .get(cursor)
                        .is_some_and(|token| is_keyword(source, token, "type"))
                {
                    children.push(token_leaf(b, &tokens[cursor]));
                    cursor += 1;
                    let type_end = scan_until_clause(tokens, cursor, period_i, clause_starts);
                    if cursor < type_end {
                        children.push(build_type_ref_node(b, source, &tokens[cursor..type_end]));
                    }
                    cursor = type_end;
                }
                if let Some(arg_list) =
                    build_call_argument_list_node(b, source, tokens, cursor, period_i)
                {
                    children.push(arg_list);
                }
                children.push(token_leaf(b, &tokens[period_i]));
            }
            CallLikeLeadKind::CreateData => {
                children.push(token_leaf(b, &tokens[idx]));
                children.push(token_leaf(b, &tokens[idx + 1]));
                let clause_starts = |tokens: &[Token], at: usize| {
                    tokens.get(at).is_some_and(|token| {
                        is_keyword(source, token, "type") || is_keyword(source, token, "like")
                    })
                };
                let mut cursor = lead_end;
                let target_end = scan_until_clause(tokens, cursor, period_i, clause_starts);
                push_expr_child(b, &mut children, source, tokens, cursor, target_end, None);
                cursor = target_end;
                if cursor < period_i
                    && tokens.get(cursor).is_some_and(|token| {
                        is_keyword(source, token, "type") || is_keyword(source, token, "like")
                    })
                {
                    children.push(token_leaf(b, &tokens[cursor]));
                    cursor += 1;
                    if cursor < period_i {
                        if tokens
                            .get(cursor - 1)
                            .is_some_and(|token| is_keyword(source, token, "like"))
                        {
                            push_expr_child(
                                b,
                                &mut children,
                                source,
                                tokens,
                                cursor,
                                period_i,
                                Some(&tokens[cursor - 1]),
                            );
                        } else if tokens.get(cursor).map(|token| token.kind)
                            == Some(TokenKind::LParen)
                            && scan_until_clause(tokens, cursor + 1, period_i, |tokens, at| {
                                tokens
                                    .get(at)
                                    .is_some_and(|token| token.kind == TokenKind::RParen)
                            }) < period_i
                        {
                            push_expr_child(
                                b,
                                &mut children,
                                source,
                                tokens,
                                cursor,
                                period_i,
                                Some(&tokens[cursor - 1]),
                            );
                        } else {
                            children.push(build_type_ref_node(
                                b,
                                source,
                                &tokens[cursor..period_i],
                            ));
                        }
                    }
                }
                children.push(token_leaf(b, &tokens[period_i]));
            }
            CallLikeLeadKind::SystemFunctionCall => {
                children.push(token_leaf(b, &tokens[idx]));
                let arg_start = scan_until_system_call_id_clause(
                    source,
                    tokens,
                    lead_end.saturating_add(1).min(period_i),
                    period_i,
                );
                push_expr_child(b, &mut children, source, tokens, lead_end, arg_start, None);
                if let Some(arg_list) = build_system_function_call_argument_list_node(
                    b, source, tokens, arg_start, period_i,
                ) {
                    children.push(arg_list);
                }
                children.push(token_leaf(b, &tokens[period_i]));
            }
            CallLikeLeadKind::CallStmt => {
                if tokens
                    .get(idx + 1)
                    .is_some_and(|token| is_keyword(source, token, "screen"))
                {
                    children = token_children(b, tokens, idx, period_i + 1);
                } else {
                    children.push(token_leaf(b, &tokens[idx]));
                    children.push(token_leaf(b, &tokens[idx + 1]));
                    let arg_start = scan_until_clause(tokens, lead_end, period_i, |tokens, at| {
                        tokens
                            .get(at)
                            .is_some_and(|_| named_argument_section_keyword(source, tokens, at))
                    });
                    for t in &tokens[lead_end..arg_start] {
                        children.push(token_leaf(b, t));
                    }
                    if let Some(arg_list) =
                        build_call_argument_list_node(b, source, tokens, arg_start, period_i)
                    {
                        children.push(arg_list);
                    }
                    children.push(token_leaf(b, &tokens[period_i]));
                }
            }
        }
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

    Some(match scan_read_table_stmt_period(tokens, source, idx + 2) {
        StmtPeriodScan::Found(period_i) => {
            let mut children = Vec::with_capacity(period_i - idx + 1);
            children.push(token_leaf(b, read_tok));
            children.push(token_leaf(b, &tokens[idx + 1]));
            if tokens
                .get(idx + 2)
                .is_some_and(|token| token.kind == TokenKind::Colon)
            {
                children.push(token_leaf(b, &tokens[idx + 2]));
                let mut cursor = idx + 3;
                let mut parsed_entry = false;
                while cursor < period_i {
                    while cursor < period_i && tokens[cursor].kind == TokenKind::Comment {
                        children.push(token_leaf(b, &tokens[cursor]));
                        cursor += 1;
                    }
                    if cursor >= period_i {
                        break;
                    }
                    let entry_end =
                        find_top_level_token_kind(tokens, cursor, period_i, TokenKind::Comma)
                            .unwrap_or(period_i);
                    let entry_start = skip_trivia(tokens, cursor);
                    if entry_start >= entry_end
                        || !push_read_table_entry_children(
                            b,
                            &mut children,
                            source,
                            tokens,
                            entry_start,
                            entry_end,
                        )
                    {
                        let raw = token_children(b, tokens, idx, period_i + 1);
                        let node = b.branch(
                            SyntaxKind::Error,
                            read_tok.range.start..tokens[period_i].range.end,
                            &raw,
                        );
                        return Some((node, period_i + 1));
                    }
                    parsed_entry = true;
                    if entry_end < period_i && tokens[entry_end].kind == TokenKind::Comma {
                        children.push(token_leaf(b, &tokens[entry_end]));
                    }
                    cursor = entry_end + 1;
                }
                if !parsed_entry {
                    let raw = token_children(b, tokens, idx, period_i + 1);
                    let node = b.branch(
                        SyntaxKind::Error,
                        read_tok.range.start..tokens[period_i].range.end,
                        &raw,
                    );
                    return Some((node, period_i + 1));
                }
            } else if !push_read_table_entry_children(
                b,
                &mut children,
                source,
                tokens,
                idx + 2,
                period_i,
            ) {
                let raw = token_children(b, tokens, idx, period_i + 1);
                let node = b.branch(
                    SyntaxKind::Error,
                    read_tok.range.start..tokens[period_i].range.end,
                    &raw,
                );
                return Some((node, period_i + 1));
            }
            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::ReadTableStmt,
                read_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        }
        StmtPeriodScan::Unterminated { end_exclusive } => {
            let err_end = unterminated_err_end(tokens, end_exclusive, read_tok.range.end);
            errors.push(crate::ParseError {
                message: "syntax error: expected '.' after READ TABLE statement".to_string(),
                range: read_tok.range.start..err_end,
            });
            let children = token_children(b, tokens, idx, end_exclusive);
            let node = b.branch(SyntaxKind::Error, read_tok.range.start..err_end, &children);
            (node, end_exclusive)
        }
    })
}

fn authority_check_stmt_clause_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    tokens.get(idx).is_some_and(|token| {
        is_keyword(source, token, "id")
            || (is_keyword(source, token, "for")
                && tokens
                    .get(idx + 1)
                    .is_some_and(|next| is_keyword(source, next, "user")))
    })
}

fn authority_check_id_clause_part_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    tokens.get(idx).is_some_and(|token| {
        is_keyword(source, token, "field")
            || is_keyword(source, token, "dummy")
            || is_keyword(source, token, "id")
    })
}

fn authority_check_field_clause_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    tokens
        .get(idx)
        .is_some_and(|token| is_keyword(source, token, "id"))
}

pub fn try_parse_authority_check_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let keyword_end = match_hyphenated_keyword(source, tokens, idx, &["authority", "check"])?;
    let authority_tok = tokens.get(idx)?;

    Some(parse_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        keyword_end,
        authority_tok,
        "syntax error: expected '.' after AUTHORITY-CHECK statement",
        errors,
        next_after_unterminated_scan,
        |b, period_i, _errors| {
            let stmt_clause_starts = |tokens: &[Token], idx: usize| {
                authority_check_stmt_clause_starts(source, tokens, idx)
            };
            let id_clause_part_starts = |tokens: &[Token], idx: usize| {
                authority_check_id_clause_part_starts(source, tokens, idx)
            };
            let field_clause_starts = |tokens: &[Token], idx: usize| {
                authority_check_field_clause_starts(source, tokens, idx)
            };

            let mut children = Vec::with_capacity(period_i - idx + 1);
            push_token_children(b, &mut children, tokens, idx, keyword_end);

            let mut i = keyword_end;
            if tokens
                .get(i)
                .is_some_and(|token| is_keyword(source, token, "object"))
            {
                let object_tok = &tokens[i];
                children.push(token_leaf(b, object_tok));
                let object_start = i + 1;
                i = scan_until_clause(tokens, object_start, period_i, &stmt_clause_starts);
                push_wrapped_expr_child(
                    b,
                    &mut children,
                    source,
                    tokens,
                    object_start,
                    i,
                    Some(object_tok),
                    SyntaxKind::AuthorityCheckObjectOperand,
                );
            }

            if tokens
                .get(i)
                .is_some_and(|token| is_keyword(source, token, "for"))
                && tokens
                    .get(i + 1)
                    .is_some_and(|token| is_keyword(source, token, "user"))
            {
                let user_tok = &tokens[i + 1];
                children.push(token_leaf(b, &tokens[i]));
                children.push(token_leaf(b, user_tok));
                let user_start = i + 2;
                i = scan_until_clause(tokens, user_start, period_i, &stmt_clause_starts);
                push_wrapped_expr_child(
                    b,
                    &mut children,
                    source,
                    tokens,
                    user_start,
                    i,
                    Some(user_tok),
                    SyntaxKind::AuthorityCheckUserOperand,
                );
            }

            while i < period_i {
                let token = &tokens[i];
                if is_keyword(source, token, "id") {
                    let mut clause_children = vec![token_leaf(b, token)];
                    let clause_start = token.range.start;
                    let id_start = i + 1;
                    let id_end =
                        scan_until_clause(tokens, id_start, period_i, &id_clause_part_starts);
                    push_wrapped_expr_child(
                        b,
                        &mut clause_children,
                        source,
                        tokens,
                        id_start,
                        id_end,
                        Some(token),
                        SyntaxKind::AuthorityCheckIdOperand,
                    );
                    i = id_end;

                    if tokens
                        .get(i)
                        .is_some_and(|field_tok| is_keyword(source, field_tok, "field"))
                    {
                        let field_tok = &tokens[i];
                        clause_children.push(token_leaf(b, field_tok));
                        let field_start = i + 1;
                        let field_end =
                            scan_until_clause(tokens, field_start, period_i, &field_clause_starts);
                        push_wrapped_expr_child(
                            b,
                            &mut clause_children,
                            source,
                            tokens,
                            field_start,
                            field_end,
                            Some(field_tok),
                            SyntaxKind::AuthorityCheckFieldOperand,
                        );
                        i = field_end;
                    } else if tokens
                        .get(i)
                        .is_some_and(|dummy_tok| is_keyword(source, dummy_tok, "dummy"))
                    {
                        clause_children.push(token_leaf(b, &tokens[i]));
                        i += 1;
                    }

                    let clause_end = clause_children
                        .last()
                        .copied()
                        .map(|child| b.span(child).end)
                        .unwrap_or(token.range.end);
                    children.push(b.branch(
                        SyntaxKind::AuthorityCheckIdClause,
                        clause_start..clause_end,
                        &clause_children,
                    ));
                    continue;
                }

                children.push(token_leaf(b, token));
                i += 1;
            }

            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::AuthorityCheckStmt,
                authority_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        },
    ))
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
    Some(parse_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        idx + 1,
        append_tok,
        "syntax error: expected '.' after APPEND statement",
        errors,
        |_, end_exclusive| end_exclusive,
        |b, period_i, _errors| {
            let clause_starts =
                |tokens: &[Token], idx: usize| append_clause_starts(source, tokens, idx);
            let Some(to_idx) =
                find_top_level_keyword_index(source, tokens, idx + 1, period_i, "to")
            else {
                let children = token_children(b, tokens, idx, period_i + 1);
                let node = b.branch(
                    SyntaxKind::Error,
                    append_tok.range.start..tokens[period_i].range.end,
                    &children,
                );
                return (node, period_i + 1);
            };
            let mut children = Vec::with_capacity(period_i - idx + 1);
            children.push(token_leaf(b, append_tok));

            let source_end = if tokens
                .get(idx + 1)
                .is_some_and(|token| is_keyword(source, token, "initial"))
                && tokens
                    .get(idx + 2)
                    .is_some_and(|token| is_keyword(source, token, "line"))
            {
                children.push(token_leaf(b, &tokens[idx + 1]));
                children.push(token_leaf(b, &tokens[idx + 2]));
                idx + 3
            } else if tokens
                .get(idx + 1)
                .is_some_and(|token| is_keyword(source, token, "lines"))
                && tokens
                    .get(idx + 2)
                    .is_some_and(|token| is_keyword(source, token, "of"))
            {
                children.push(token_leaf(b, &tokens[idx + 1]));
                children.push(token_leaf(b, &tokens[idx + 2]));
                scan_and_push_expr_clause(
                    b,
                    &mut children,
                    source,
                    tokens,
                    idx + 3,
                    to_idx,
                    tokens.get(idx + 2),
                    &clause_starts,
                )
            } else {
                scan_and_push_expr_clause(
                    b,
                    &mut children,
                    source,
                    tokens,
                    idx + 1,
                    to_idx,
                    Some(append_tok),
                    &clause_starts,
                )
            };
            push_token_children(b, &mut children, tokens, source_end, to_idx);

            children.push(token_leaf(b, &tokens[to_idx]));
            let mut i = to_idx + 1;
            i = scan_and_push_expr_clause(
                b,
                &mut children,
                source,
                tokens,
                i,
                period_i,
                Some(&tokens[to_idx]),
                &clause_starts,
            );

            while i < period_i {
                let token = &tokens[i];
                if is_keyword(source, token, "assigning") {
                    children.push(token_leaf(b, token));
                    i = scan_and_push_assigning_target_clause(
                        b,
                        &mut children,
                        source,
                        tokens,
                        i + 1,
                        period_i,
                        token,
                        &clause_starts,
                    );
                    continue;
                } else if is_keyword(source, token, "reference")
                    && tokens
                        .get(i + 1)
                        .is_some_and(|next| is_keyword(source, next, "into"))
                {
                    i = scan_and_push_reference_into_clause(
                        b,
                        &mut children,
                        source,
                        tokens,
                        i + 1,
                        period_i,
                        &clause_starts,
                    );
                    continue;
                } else if is_keyword(source, token, "sorted") {
                    children.push(token_leaf(b, token));
                    i = scan_and_push_expr_clause(
                        b,
                        &mut children,
                        source,
                        tokens,
                        i + 1,
                        period_i,
                        Some(token),
                        &clause_starts,
                    );
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
            (node, period_i + 1)
        },
    ))
}

pub fn try_parse_insert_table_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let insert_tok = tokens.get(idx)?;
    if !is_keyword(source, insert_tok, "insert") {
        return None;
    }
    match scan_until_statement_period(tokens, source, idx + 1) {
        StmtPeriodScan::Found(period_i) => {
            if tokens
                .get(idx + 1)
                .is_some_and(|token| is_keyword(source, token, "textpool"))
            {
                let node = build_insert_textpool_stmt(b, source, tokens, idx, period_i);
                return Some((node, period_i + 1));
            }
            let Some(into_idx) =
                find_top_level_keyword_index(source, tokens, idx + 1, period_i, "into")
            else {
                let Some(_from_idx) =
                    find_top_level_keyword_index(source, tokens, idx + 1, period_i, "from")
                else {
                    return None;
                };
                let target_clause =
                    |tokens: &[Token], i: usize| insert_db_table_clause_starts(source, tokens, i);
                let target_end = scan_until_clause(tokens, idx + 1, period_i, &target_clause);
                let node = build_insert_db_table_stmt(
                    b,
                    source,
                    tokens,
                    idx,
                    period_i,
                    None,
                    idx + 1,
                    target_end,
                );
                return Some((node, period_i + 1));
            };
            if let Some(target_end) =
                find_insert_into_db_table_target_end(source, tokens, into_idx + 1, period_i)
            {
                let node = build_insert_db_table_stmt(
                    b,
                    source,
                    tokens,
                    idx,
                    period_i,
                    Some(into_idx),
                    into_idx + 1,
                    target_end,
                );
                return Some((node, period_i + 1));
            }
            let has_table_kw = tokens
                .get(into_idx + 1)
                .is_some_and(|t| is_keyword(source, t, "table"));
            if !has_table_kw {
                let itab_start = into_idx + 1;
                let bare_clause =
                    |t: &[Token], i: usize| insert_into_bare_itab_clause_starts(source, t, i);
                let itab_end = scan_until_clause(tokens, itab_start, period_i, &bare_clause);
                if itab_end < period_i {
                    let head = &tokens[itab_end];
                    if is_keyword(source, head, "values") || is_keyword(source, head, "set") {
                        return None;
                    }
                }
            }
            let mut children = Vec::with_capacity(period_i - idx + 1);
            children.push(token_leaf(b, insert_tok));
            let source_clause = |tokens: &[Token], i: usize| {
                insert_internal_source_clause_starts(source, tokens, i)
            };
            scan_and_push_expr_clause(
                b,
                &mut children,
                source,
                tokens,
                idx + 1,
                into_idx,
                Some(insert_tok),
                &source_clause,
            );
            children.push(token_leaf(b, &tokens[into_idx]));
            let (table_expr_start, prev_before_itab): (usize, &Token) = if has_table_kw {
                children.push(token_leaf(b, &tokens[into_idx + 1]));
                (into_idx + 2, &tokens[into_idx + 1])
            } else {
                (into_idx + 1, &tokens[into_idx])
            };
            let tail_clause = |tokens: &[Token], i: usize| {
                insert_into_table_tail_clause_starts(source, tokens, i)
            };
            let mut i = scan_and_push_expr_clause(
                b,
                &mut children,
                source,
                tokens,
                table_expr_start,
                period_i,
                Some(prev_before_itab),
                &tail_clause,
            );
            while i < period_i {
                let token = &tokens[i];
                if is_keyword(source, token, "index") {
                    children.push(token_leaf(b, token));
                    i = scan_and_push_expr_clause(
                        b,
                        &mut children,
                        source,
                        tokens,
                        i + 1,
                        period_i,
                        Some(token),
                        &tail_clause,
                    );
                    continue;
                }
                if is_keyword(source, token, "assigning") {
                    children.push(token_leaf(b, token));
                    i = scan_and_push_assigning_target_clause(
                        b,
                        &mut children,
                        source,
                        tokens,
                        i + 1,
                        period_i,
                        token,
                        &tail_clause,
                    );
                    continue;
                } else if is_keyword(source, token, "reference")
                    && tokens
                        .get(i + 1)
                        .is_some_and(|next| is_keyword(source, next, "into"))
                {
                    i = scan_and_push_reference_into_clause(
                        b,
                        &mut children,
                        source,
                        tokens,
                        i + 1,
                        period_i,
                        &tail_clause,
                    );
                    continue;
                }
                children.push(token_leaf(b, token));
                i += 1;
            }
            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::InsertTableStmt,
                insert_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            Some((node, period_i + 1))
        }
        StmtPeriodScan::Unterminated { end_exclusive } => {
            let err_end = unterminated_err_end(tokens, end_exclusive, insert_tok.range.end);
            errors.push(crate::ParseError {
                message: "syntax error: expected '.' after INSERT statement".to_string(),
                range: insert_tok.range.start..err_end,
            });
            let children = token_children(b, tokens, idx, end_exclusive);
            let node = b.branch(
                SyntaxKind::Error,
                insert_tok.range.start..err_end,
                &children,
            );
            Some((node, next_after_unterminated_scan(tokens, end_exclusive)))
        }
    }
}

fn move_corresponding_clause_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    token.kind == TokenKind::Ident
        && (is_keyword(source, token, "to")
            || is_keyword(source, token, "expanding")
            || is_keyword(source, token, "keeping"))
}

pub fn try_parse_move_corresponding_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let move_tok = tokens.get(idx)?;
    let keyword_end = match_hyphenated_keyword(source, tokens, idx, &["move", "corresponding"])?;
    Some(parse_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        keyword_end,
        move_tok,
        "syntax error: expected '.' after MOVE-CORRESPONDING statement",
        errors,
        |_, end_exclusive| end_exclusive,
        |b, period_i, _errors| {
            let clause_starts = |tokens: &[Token], idx: usize| {
                move_corresponding_clause_starts(source, tokens, idx)
            };
            let Some(to_idx) =
                find_top_level_keyword_index(source, tokens, keyword_end, period_i, "to")
            else {
                let children = token_children(b, tokens, idx, period_i + 1);
                let node = b.branch(
                    SyntaxKind::Error,
                    move_tok.range.start..tokens[period_i].range.end,
                    &children,
                );
                return (node, period_i + 1);
            };

            let mut children = Vec::with_capacity(period_i - idx + 1);
            push_token_children(b, &mut children, tokens, idx, keyword_end);

            let source_end = scan_and_push_expr_clause(
                b,
                &mut children,
                source,
                tokens,
                keyword_end,
                to_idx,
                tokens.get(keyword_end.saturating_sub(1)),
                &clause_starts,
            );
            push_token_children(b, &mut children, tokens, source_end, to_idx);

            children.push(token_leaf(b, &tokens[to_idx]));
            let mut i = scan_and_push_expr_clause(
                b,
                &mut children,
                source,
                tokens,
                to_idx + 1,
                period_i,
                Some(&tokens[to_idx]),
                &clause_starts,
            );

            while i < period_i {
                children.push(token_leaf(b, &tokens[i]));
                i += 1;
            }
            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::MoveCorrespondingStmt,
                move_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        },
    ))
}

fn move_simple_source_clause_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    token.kind == TokenKind::Ident && is_keyword(source, token, "to")
}

fn push_move_entry_children(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    source: &str,
    tokens: &[Token],
    entry_start: usize,
    entry_end: usize,
) {
    let mut cursor = entry_start;
    while cursor < entry_end && tokens[cursor].kind == TokenKind::Comment {
        children.push(token_leaf(b, &tokens[cursor]));
        cursor += 1;
    }
    if cursor >= entry_end {
        return;
    }

    let clause_starts =
        |tokens: &[Token], idx: usize| move_simple_source_clause_starts(source, tokens, idx);
    let Some(to_idx) = find_top_level_keyword_index(source, tokens, cursor, entry_end, "to") else {
        push_token_children(b, children, tokens, cursor, entry_end);
        return;
    };

    let source_end = scan_and_push_expr_clause(
        b,
        children,
        source,
        tokens,
        cursor,
        to_idx,
        tokens.get(cursor.saturating_sub(1)),
        &clause_starts,
    );
    push_token_children(b, children, tokens, source_end, to_idx);
    children.push(token_leaf(b, &tokens[to_idx]));

    let no_clause = |_: &[Token], _: usize| false;
    let target_end = scan_and_push_expr_clause(
        b,
        children,
        source,
        tokens,
        to_idx + 1,
        entry_end,
        Some(&tokens[to_idx]),
        &no_clause,
    );
    push_token_children(b, children, tokens, target_end, entry_end);
}

fn push_chained_move_entries(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) {
    let mut cursor = start;
    while cursor < end_exclusive {
        let token = &tokens[cursor];
        if matches!(
            token.kind,
            TokenKind::Colon | TokenKind::Comma | TokenKind::Comment
        ) {
            children.push(token_leaf(b, token));
            cursor += 1;
            continue;
        }

        let entry_end = find_top_level_token_kind(tokens, cursor, end_exclusive, TokenKind::Comma)
            .unwrap_or(end_exclusive);
        push_move_entry_children(b, children, source, tokens, cursor, entry_end);
        cursor = entry_end;
    }
}

/// `MOVE ... TO ...` and `MOVE-CORRESPONDING ...` (delegates to [`try_parse_move_corresponding_stmt`]).
pub fn try_parse_move_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    if let Some(parsed) = try_parse_move_corresponding_stmt(b, source, tokens, idx, errors) {
        return Some(parsed);
    }
    let move_tok = tokens.get(idx)?;
    if !is_keyword(source, move_tok, "move") {
        return None;
    }
    Some(parse_chained_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        idx + 1,
        move_tok,
        "syntax error: expected '.' after MOVE statement",
        errors,
        |_, end_exclusive| end_exclusive,
        |b, period_i, _errors| {
            if tokens
                .get(skip_trivia(tokens, idx + 1))
                .is_some_and(|token| token.kind == TokenKind::Colon)
            {
                let mut children = Vec::with_capacity(period_i - idx + 1);
                children.push(token_leaf(b, move_tok));
                push_chained_move_entries(b, &mut children, source, tokens, idx + 1, period_i);
                children.push(token_leaf(b, &tokens[period_i]));
                let node = b.branch(
                    SyntaxKind::MoveStmt,
                    move_tok.range.start..tokens[period_i].range.end,
                    &children,
                );
                return (node, period_i + 1);
            }

            let clause_starts = |tokens: &[Token], idx: usize| {
                move_simple_source_clause_starts(source, tokens, idx)
            };
            let Some(to_idx) =
                find_top_level_keyword_index(source, tokens, idx + 1, period_i, "to")
            else {
                let children = token_children(b, tokens, idx, period_i + 1);
                let node = b.branch(
                    SyntaxKind::Error,
                    move_tok.range.start..tokens[period_i].range.end,
                    &children,
                );
                return (node, period_i + 1);
            };
            let mut children = Vec::with_capacity(period_i - idx + 1);
            children.push(token_leaf(b, move_tok));
            let source_end = scan_and_push_expr_clause(
                b,
                &mut children,
                source,
                tokens,
                idx + 1,
                to_idx,
                Some(move_tok),
                &clause_starts,
            );
            push_token_children(b, &mut children, tokens, source_end, to_idx);
            children.push(token_leaf(b, &tokens[to_idx]));
            let no_clause = |_: &[Token], _: usize| false;
            let mut i = scan_and_push_expr_clause(
                b,
                &mut children,
                source,
                tokens,
                to_idx + 1,
                period_i,
                Some(&tokens[to_idx]),
                &no_clause,
            );
            while i < period_i {
                children.push(token_leaf(b, &tokens[i]));
                i += 1;
            }
            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::MoveStmt,
                move_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        },
    ))
}

fn sort_modifier_clause_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    if token.kind != TokenKind::Ident {
        return false;
    }
    is_keyword(source, token, "stable")
        || is_keyword(source, token, "by")
        || (is_keyword(source, token, "as")
            && tokens
                .get(idx + 1)
                .is_some_and(|next| is_keyword(source, next, "text")))
}

fn sort_modifier_before_period_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    if token.kind != TokenKind::Ident {
        return false;
    }
    is_keyword(source, token, "stable")
        || (is_keyword(source, token, "as")
            && tokens
                .get(idx + 1)
                .is_some_and(|next| is_keyword(source, next, "text")))
}

fn sort_key_modifier_starts(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    token.kind == TokenKind::Ident
        && (is_keyword(source, token, "ascending")
            || is_keyword(source, token, "descending")
            || (is_keyword(source, token, "as")
                && tokens
                    .get(idx + 1)
                    .is_some_and(|next| is_keyword(source, next, "text"))))
}

fn token_starts_sort_by_operand(source: &str, tokens: &[Token], idx: usize) -> bool {
    let Some(token) = tokens.get(idx) else {
        return false;
    };
    if !matches!(
        token.kind,
        TokenKind::Ident
            | TokenKind::Number
            | TokenKind::String
            | TokenKind::StringTemplate
            | TokenKind::LParen
            | TokenKind::LBracket
            | TokenKind::LBrace
            | TokenKind::At
            | TokenKind::Hash
    ) {
        return false;
    }
    if token.kind == TokenKind::Ident
        && (is_keyword(source, token, "ascending")
            || is_keyword(source, token, "descending")
            || is_keyword(source, token, "as"))
    {
        return false;
    }
    let Some(prev) = idx.checked_sub(1).and_then(|prev_idx| tokens.get(prev_idx)) else {
        return true;
    };
    have_space_between(prev, token)
        && !matches!(
            prev.kind,
            TokenKind::Arrow
                | TokenKind::FatArrow
                | TokenKind::Tilde
                | TokenKind::Eq
                | TokenKind::Minus
                | TokenKind::Plus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::Lt
                | TokenKind::Gt
                | TokenKind::Le
                | TokenKind::Ge
                | TokenKind::Ne
                | TokenKind::QuestionEq
                | TokenKind::LParen
                | TokenKind::LBracket
                | TokenKind::LBrace
                | TokenKind::At
                | TokenKind::Hash
                | TokenKind::Ampersand
                | TokenKind::Pipe
        )
}

fn consume_sort_by_operand(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> usize {
    let mut idx = start;
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut consumed_any = false;

    while idx < end_exclusive {
        let token = &tokens[idx];
        if paren == 0 && bracket == 0 && brace == 0 {
            if sort_key_modifier_starts(source, tokens, idx) {
                break;
            }
            if consumed_any && token_starts_sort_by_operand(source, tokens, idx) {
                break;
            }
        }

        consumed_any = true;
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

pub fn try_parse_sort_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let sort_tok = tokens.get(idx)?;
    if !is_keyword(source, sort_tok, "sort") {
        return None;
    }
    Some(parse_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        idx + 1,
        sort_tok,
        "syntax error: expected '.' after SORT statement",
        errors,
        |_, end_exclusive| end_exclusive,
        |b, period_i, _errors| {
            let by_idx = find_top_level_keyword_index(source, tokens, idx + 1, period_i, "by");
            let mut children = Vec::with_capacity(period_i - idx + 1);
            children.push(token_leaf(b, sort_tok));

            if let Some(by_idx) = by_idx {
                let mut cur = idx + 1;
                while cur < by_idx {
                    cur = scan_and_push_expr_clause(
                        b,
                        &mut children,
                        source,
                        tokens,
                        cur,
                        by_idx,
                        Some(sort_tok),
                        &|t, i| sort_modifier_clause_starts(source, t, i),
                    );
                    if cur >= by_idx {
                        break;
                    }
                    let token = &tokens[cur];
                    if is_keyword(source, token, "stable") {
                        children.push(token_leaf(b, token));
                        cur += 1;
                    } else if is_keyword(source, token, "as")
                        && tokens
                            .get(cur + 1)
                            .is_some_and(|next| is_keyword(source, next, "text"))
                    {
                        children.push(token_leaf(b, token));
                        children.push(token_leaf(b, &tokens[cur + 1]));
                        cur += 2;
                    } else {
                        children.push(token_leaf(b, token));
                        cur += 1;
                    }
                }
                push_token_children(b, &mut children, tokens, cur, by_idx);
                children.push(token_leaf(b, &tokens[by_idx]));
                let mut tail = by_idx + 1;
                while tail < period_i {
                    if matches!(tokens[tail].kind, TokenKind::Comment) {
                        children.push(token_leaf(b, &tokens[tail]));
                        tail += 1;
                        continue;
                    }

                    let operand_end = consume_sort_by_operand(source, tokens, tail, period_i);
                    if operand_end == tail {
                        children.push(token_leaf(b, &tokens[tail]));
                        tail += 1;
                    } else {
                        push_expr_child(
                            b,
                            &mut children,
                            source,
                            tokens,
                            tail,
                            operand_end,
                            tail.checked_sub(1).and_then(|idx| tokens.get(idx)),
                        );
                        tail = operand_end;
                    }

                    while tail < period_i {
                        let token = &tokens[tail];
                        if matches!(token.kind, TokenKind::Comment) {
                            children.push(token_leaf(b, token));
                            tail += 1;
                        } else if is_keyword(source, token, "ascending")
                            || is_keyword(source, token, "descending")
                        {
                            children.push(token_leaf(b, token));
                            tail += 1;
                        } else if is_keyword(source, token, "as")
                            && tokens
                                .get(tail + 1)
                                .is_some_and(|next| is_keyword(source, next, "text"))
                        {
                            children.push(token_leaf(b, token));
                            children.push(token_leaf(b, &tokens[tail + 1]));
                            tail += 2;
                        } else {
                            break;
                        }
                    }
                }
            } else {
                let mut cur = idx + 1;
                while cur < period_i {
                    cur = scan_and_push_expr_clause(
                        b,
                        &mut children,
                        source,
                        tokens,
                        cur,
                        period_i,
                        Some(sort_tok),
                        &|t, i| sort_modifier_before_period_starts(source, t, i),
                    );
                    if cur >= period_i {
                        break;
                    }
                    let token = &tokens[cur];
                    if is_keyword(source, token, "stable") {
                        children.push(token_leaf(b, token));
                        cur += 1;
                    } else if is_keyword(source, token, "as")
                        && tokens
                            .get(cur + 1)
                            .is_some_and(|next| is_keyword(source, next, "text"))
                    {
                        children.push(token_leaf(b, token));
                        children.push(token_leaf(b, &tokens[cur + 1]));
                        cur += 2;
                    } else {
                        children.push(token_leaf(b, token));
                        cur += 1;
                    }
                }
            }

            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::SortStmt,
                sort_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        },
    ))
}

pub fn try_parse_modify_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let modify_tok = tokens.get(idx)?;
    if !is_keyword(source, modify_tok, "modify") {
        return None;
    }
    Some(parse_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        idx + 1,
        modify_tok,
        "syntax error: expected '.' after MODIFY statement",
        errors,
        |_, end_exclusive| end_exclusive,
        |b, period_i, _errors| {
            if tokens
                .get(idx + 1)
                .is_some_and(|token| is_keyword(source, token, "screen"))
            {
                let children = token_children(b, tokens, idx, period_i + 1);
                let node = b.branch(
                    SyntaxKind::ModifyStmt,
                    modify_tok.range.start..tokens[period_i].range.end,
                    &children,
                );
                return (node, period_i + 1);
            }

            let clause_starts =
                |tokens: &[Token], idx: usize| modify_clause_starts(source, tokens, idx);
            let Some(from_idx) =
                find_top_level_keyword_index(source, tokens, idx + 1, period_i, "from")
            else {
                let children = token_children(b, tokens, idx, period_i + 1);
                let node = b.branch(
                    SyntaxKind::Error,
                    modify_tok.range.start..tokens[period_i].range.end,
                    &children,
                );
                return (node, period_i + 1);
            };

            let mut children = Vec::with_capacity(period_i - idx + 1);
            children.push(token_leaf(b, modify_tok));

            let mut target_start = idx + 1;
            if tokens
                .get(target_start)
                .is_some_and(|token| is_keyword(source, token, "table"))
            {
                children.push(token_leaf(b, &tokens[target_start]));
                target_start += 1;
            }

            let target_end = scan_and_push_expr_clause(
                b,
                &mut children,
                source,
                tokens,
                target_start,
                from_idx,
                Some(modify_tok),
                &clause_starts,
            );
            push_token_children(b, &mut children, tokens, target_end, from_idx);

            children.push(token_leaf(b, &tokens[from_idx]));
            let mut i = from_idx + 1;
            if tokens
                .get(i)
                .is_some_and(|token| is_keyword(source, token, "table"))
            {
                children.push(token_leaf(b, &tokens[i]));
                i += 1;
            }
            i = scan_and_push_expr_clause(
                b,
                &mut children,
                source,
                tokens,
                i,
                period_i,
                Some(&tokens[from_idx]),
                &clause_starts,
            );

            while i < period_i {
                let token = &tokens[i];
                if is_keyword(source, token, "index") || is_keyword(source, token, "where") {
                    children.push(token_leaf(b, token));
                    i = scan_and_push_expr_clause(
                        b,
                        &mut children,
                        source,
                        tokens,
                        i + 1,
                        period_i,
                        Some(token),
                        &clause_starts,
                    );
                    continue;
                }
                if is_keyword(source, token, "transporting") {
                    children.push(token_leaf(b, token));
                    i += 1;
                    while i < period_i && !clause_starts(tokens, i) {
                        children.push(token_leaf(b, &tokens[i]));
                        i += 1;
                    }
                    continue;
                }
                children.push(token_leaf(b, token));
                i += 1;
            }

            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::ModifyStmt,
                modify_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        },
    ))
}

pub fn try_parse_delete_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let delete_tok = tokens.get(idx)?;
    if !is_keyword(source, delete_tok, "delete") {
        return None;
    }
    Some(parse_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        idx + 1,
        delete_tok,
        "syntax error: expected '.' after DELETE statement",
        errors,
        |_, end_exclusive| end_exclusive,
        |b, period_i, _errors| {
            if tokens
                .get(idx + 1)
                .is_some_and(|token| is_keyword(source, token, "dataset"))
            {
                let children = token_children(b, tokens, idx, period_i + 1);
                let node = b.branch(
                    SyntaxKind::DeleteDatasetStmt,
                    delete_tok.range.start..tokens[period_i].range.end,
                    &children,
                );
                return (node, period_i + 1);
            }

            let clause_starts =
                |tokens: &[Token], idx: usize| delete_clause_starts(source, tokens, idx);
            let stmt_kind = delete_stmt_kind(source, tokens, idx + 1, period_i);
            let mut children = Vec::with_capacity(period_i - idx + 1);
            children.push(token_leaf(b, delete_tok));

            let mut i = idx + 1;
            if tokens
                .get(i)
                .is_some_and(|token| is_keyword(source, token, "adjacent"))
                && tokens
                    .get(i + 1)
                    .is_some_and(|token| is_keyword(source, token, "duplicates"))
            {
                children.push(token_leaf(b, &tokens[i]));
                children.push(token_leaf(b, &tokens[i + 1]));
                i += 2;
            } else if tokens
                .get(i)
                .is_some_and(|token| is_keyword(source, token, "table"))
            {
                children.push(token_leaf(b, &tokens[i]));
                i += 1;
                i = scan_and_push_expr_clause(
                    b,
                    &mut children,
                    source,
                    tokens,
                    i,
                    period_i,
                    Some(&tokens[i - 1]),
                    &clause_starts,
                );
            } else {
                i = scan_and_push_expr_clause(
                    b,
                    &mut children,
                    source,
                    tokens,
                    i,
                    period_i,
                    Some(delete_tok),
                    &clause_starts,
                );
            }

            while i < period_i {
                let token = &tokens[i];
                if is_keyword(source, token, "from") {
                    children.push(token_leaf(b, token));
                    i += 1;
                    if tokens
                        .get(i)
                        .is_some_and(|next| is_keyword(source, next, "table"))
                    {
                        children.push(token_leaf(b, &tokens[i]));
                        i += 1;
                    }
                    i = scan_and_push_expr_clause(
                        b,
                        &mut children,
                        source,
                        tokens,
                        i,
                        period_i,
                        Some(token),
                        &clause_starts,
                    );
                    continue;
                }
                if is_keyword(source, token, "where") {
                    children.push(token_leaf(b, token));
                    i = scan_and_push_logical_expr_clause(
                        b,
                        &mut children,
                        source,
                        tokens,
                        i + 1,
                        period_i,
                        Some(token),
                        &clause_starts,
                    );
                    continue;
                }
                if is_keyword(source, token, "index") {
                    children.push(token_leaf(b, token));
                    i = scan_and_push_expr_clause(
                        b,
                        &mut children,
                        source,
                        tokens,
                        i + 1,
                        period_i,
                        Some(token),
                        &clause_starts,
                    );
                    continue;
                }
                if is_keyword(source, token, "using")
                    && tokens
                        .get(i + 1)
                        .is_some_and(|next| is_keyword(source, next, "key"))
                {
                    children.push(token_leaf(b, token));
                    children.push(token_leaf(b, &tokens[i + 1]));
                    i = scan_and_push_expr_clause(
                        b,
                        &mut children,
                        source,
                        tokens,
                        i + 2,
                        period_i,
                        Some(&tokens[i + 1]),
                        &clause_starts,
                    );
                    continue;
                }
                if is_keyword(source, token, "comparing") {
                    children.push(token_leaf(b, token));
                    i += 1;
                    if tokens
                        .get(i)
                        .is_some_and(|next| is_keyword(source, next, "all"))
                        && tokens
                            .get(i + 1)
                            .is_some_and(|next| is_keyword(source, next, "fields"))
                    {
                        children.push(token_leaf(b, &tokens[i]));
                        children.push(token_leaf(b, &tokens[i + 1]));
                        i += 2;
                        continue;
                    }
                    while i < period_i && !clause_starts(tokens, i) {
                        let operand_end = consume_sort_by_operand(source, tokens, i, period_i);
                        if operand_end == i {
                            children.push(token_leaf(b, &tokens[i]));
                            i += 1;
                            continue;
                        }
                        i = scan_and_push_expr_clause(
                            b,
                            &mut children,
                            source,
                            tokens,
                            i,
                            operand_end,
                            Some(token),
                            &clause_starts,
                        );
                    }
                    continue;
                }
                children.push(token_leaf(b, token));
                i += 1;
            }

            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                stmt_kind,
                delete_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        },
    ))
}

fn push_classic_operand_entry(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    source: &str,
    tokens: &[Token],
    entry_start: usize,
    entry_end: usize,
    wrapper_kind: SyntaxKind,
    prefix_keywords: &[&str],
) {
    let mut operand_start = entry_start;
    while operand_start < entry_end {
        let token = &tokens[operand_start];
        if token.kind == TokenKind::Comment
            || prefix_keywords
                .iter()
                .any(|keyword| is_keyword(source, token, keyword))
        {
            children.push(token_leaf(b, token));
            operand_start += 1;
            continue;
        }
        break;
    }

    if operand_start < entry_end {
        push_wrapped_expr_child(
            b,
            children,
            source,
            tokens,
            operand_start,
            entry_end,
            tokens.get(operand_start.saturating_sub(1)),
            wrapper_kind,
        );
    }
}

fn push_chained_classic_operands(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
    wrapper_kind: SyntaxKind,
    prefix_keywords: &[&str],
) {
    let mut cursor = start;
    while cursor < end_exclusive {
        let token = &tokens[cursor];
        if matches!(
            token.kind,
            TokenKind::Colon | TokenKind::Comma | TokenKind::Comment
        ) {
            children.push(token_leaf(b, token));
            cursor += 1;
            continue;
        }

        let entry_end = find_top_level_token_kind(tokens, cursor, end_exclusive, TokenKind::Comma)
            .unwrap_or(end_exclusive);
        push_classic_operand_entry(
            b,
            children,
            source,
            tokens,
            cursor,
            entry_end,
            wrapper_kind,
            prefix_keywords,
        );
        cursor = entry_end;
    }
}

fn push_memory_id_sequence_and_operand(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    source: &str,
    tokens: &[Token],
    sequence_start: usize,
    sequence_end: usize,
    period_i: usize,
) {
    push_cluster_sequence_and_operand(
        b,
        children,
        source,
        tokens,
        sequence_start,
        sequence_end,
        period_i,
        SyntaxKind::MemoryIdOperand,
    );
}

fn push_cluster_sequence_and_operand(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    source: &str,
    tokens: &[Token],
    sequence_start: usize,
    sequence_end: usize,
    period_i: usize,
    operand_kind: SyntaxKind,
) {
    push_token_children(b, children, tokens, sequence_start, sequence_end);
    push_wrapped_expr_child(
        b,
        children,
        source,
        tokens,
        sequence_end,
        period_i,
        tokens.get(sequence_end.saturating_sub(1)),
        operand_kind,
    );
}

fn find_import_cluster_sequence_index(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> Option<(usize, usize, SyntaxKind)> {
    find_top_level_keyword_sequence_index(
        source,
        tokens,
        start,
        end_exclusive,
        &["from", "memory", "id"],
    )
    .map(|(sequence_start, operand_start)| {
        (sequence_start, operand_start, SyntaxKind::MemoryIdOperand)
    })
    .or_else(|| {
        find_top_level_keyword_sequence_index(
            source,
            tokens,
            start,
            end_exclusive,
            &["from", "data", "buffer"],
        )
        .map(|(sequence_start, operand_start)| {
            (sequence_start, operand_start, SyntaxKind::DataBufferOperand)
        })
    })
}

fn find_export_cluster_sequence_index(
    source: &str,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> Option<(usize, usize, SyntaxKind)> {
    find_top_level_keyword_sequence_index(
        source,
        tokens,
        start,
        end_exclusive,
        &["to", "memory", "id"],
    )
    .map(|(sequence_start, operand_start)| {
        (sequence_start, operand_start, SyntaxKind::MemoryIdOperand)
    })
    .or_else(|| {
        find_top_level_keyword_sequence_index(
            source,
            tokens,
            start,
            end_exclusive,
            &["to", "data", "buffer"],
        )
        .map(|(sequence_start, operand_start)| {
            (sequence_start, operand_start, SyntaxKind::DataBufferOperand)
        })
    })
}

pub fn try_parse_refresh_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let refresh_tok = tokens.get(idx)?;
    if !is_keyword(source, refresh_tok, "refresh") {
        return None;
    }

    Some(parse_chained_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        idx + 1,
        refresh_tok,
        "syntax error: expected '.' after REFRESH statement",
        errors,
        next_after_unterminated_scan,
        |b, period_i, _errors| {
            let mut children = vec![token_leaf(b, refresh_tok)];
            push_chained_classic_operands(
                b,
                &mut children,
                source,
                tokens,
                idx + 1,
                period_i,
                SyntaxKind::RefreshOperand,
                &["table"],
            );
            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::RefreshStmt,
                refresh_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        },
    ))
}

fn push_collect_entry_children(
    b: &mut SyntaxTreeBuilder,
    children: &mut Vec<NodeId>,
    source: &str,
    tokens: &[Token],
    entry_start: usize,
    entry_end: usize,
) {
    let entry_start = skip_trivia(tokens, entry_start);
    if entry_start >= entry_end {
        return;
    }

    if let Some(into_idx) =
        find_top_level_keyword_index(source, tokens, entry_start, entry_end, "into")
    {
        push_wrapped_expr_child(
            b,
            children,
            source,
            tokens,
            entry_start,
            into_idx,
            tokens.get(entry_start.saturating_sub(1)),
            SyntaxKind::CollectSourceOperand,
        );
        children.push(token_leaf(b, &tokens[into_idx]));
        push_wrapped_expr_child(
            b,
            children,
            source,
            tokens,
            into_idx + 1,
            entry_end,
            Some(&tokens[into_idx]),
            SyntaxKind::CollectTargetOperand,
        );
    } else {
        push_wrapped_expr_child(
            b,
            children,
            source,
            tokens,
            entry_start,
            entry_end,
            tokens.get(entry_start.saturating_sub(1)),
            SyntaxKind::CollectSourceOperand,
        );
    }
}

pub fn try_parse_collect_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let collect_tok = tokens.get(idx)?;
    if !is_keyword(source, collect_tok, "collect") {
        return None;
    }

    Some(parse_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        idx + 1,
        collect_tok,
        "syntax error: expected '.' after COLLECT statement",
        errors,
        next_after_unterminated_scan,
        |b, period_i, _errors| {
            let mut children = vec![token_leaf(b, collect_tok)];
            let mut cursor = idx + 1;
            while cursor < period_i {
                let token = &tokens[cursor];
                if matches!(
                    token.kind,
                    TokenKind::Colon | TokenKind::Comma | TokenKind::Comment
                ) {
                    children.push(token_leaf(b, token));
                    cursor += 1;
                    continue;
                }

                let entry_end =
                    find_top_level_token_kind(tokens, cursor, period_i, TokenKind::Comma)
                        .unwrap_or(period_i);
                push_collect_entry_children(b, &mut children, source, tokens, cursor, entry_end);
                cursor = entry_end;
            }
            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::CollectStmt,
                collect_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        },
    ))
}

pub fn try_parse_free_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let free_tok = tokens.get(idx)?;
    if !is_keyword(source, free_tok, "free") {
        return None;
    }

    Some(parse_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        idx + 1,
        free_tok,
        "syntax error: expected '.' after FREE statement",
        errors,
        next_after_unterminated_scan,
        |b, period_i, _errors| {
            let mut children = vec![token_leaf(b, free_tok)];
            if let Some((memory_idx, memory_id_start)) = find_top_level_keyword_sequence_index(
                source,
                tokens,
                idx + 1,
                period_i,
                &["memory", "id"],
            ) {
                push_memory_id_sequence_and_operand(
                    b,
                    &mut children,
                    source,
                    tokens,
                    memory_idx,
                    memory_id_start,
                    period_i,
                );
            } else {
                push_chained_classic_operands(
                    b,
                    &mut children,
                    source,
                    tokens,
                    idx + 1,
                    period_i,
                    SyntaxKind::FreeOperand,
                    &["object"],
                );
            }
            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::FreeStmt,
                free_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        },
    ))
}

pub fn try_parse_unassign_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let unassign_tok = tokens.get(idx)?;
    if !is_keyword(source, unassign_tok, "unassign") {
        return None;
    }

    Some(parse_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        idx + 1,
        unassign_tok,
        "syntax error: expected '.' after UNASSIGN statement",
        errors,
        next_after_unterminated_scan,
        |b, period_i, _errors| {
            let mut children = vec![token_leaf(b, unassign_tok)];
            push_chained_classic_operands(
                b,
                &mut children,
                source,
                tokens,
                idx + 1,
                period_i,
                SyntaxKind::UnassignOperand,
                &[],
            );
            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::UnassignStmt,
                unassign_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        },
    ))
}

pub fn try_parse_import_memory_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let import_tok = tokens.get(idx)?;
    if !is_keyword(source, import_tok, "import") {
        return None;
    }

    match scan_until_statement_period_with_named_args(tokens, source, idx + 1, true) {
        StmtPeriodScan::Found(period_i) => {
            let (from_idx, operand_start, operand_kind) =
                find_import_cluster_sequence_index(source, tokens, idx + 1, period_i)?;
            let mut children = vec![token_leaf(b, import_tok)];
            if let Some(to_idx) =
                find_top_level_keyword_index(source, tokens, idx + 1, from_idx, "to")
            {
                push_wrapped_expr_child(
                    b,
                    &mut children,
                    source,
                    tokens,
                    idx + 1,
                    to_idx,
                    Some(import_tok),
                    SyntaxKind::ImportMemorySourceOperand,
                );
                children.push(token_leaf(b, &tokens[to_idx]));
                push_wrapped_expr_child(
                    b,
                    &mut children,
                    source,
                    tokens,
                    to_idx + 1,
                    from_idx,
                    Some(&tokens[to_idx]),
                    SyntaxKind::ImportMemoryTargetOperand,
                );
            } else {
                push_wrapped_expr_child(
                    b,
                    &mut children,
                    source,
                    tokens,
                    idx + 1,
                    from_idx,
                    Some(import_tok),
                    SyntaxKind::ImportMemoryTargetOperand,
                );
            }
            push_cluster_sequence_and_operand(
                b,
                &mut children,
                source,
                tokens,
                from_idx,
                operand_start,
                period_i,
                operand_kind,
            );
            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::ImportMemoryStmt,
                import_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            Some((node, period_i + 1))
        }
        StmtPeriodScan::Unterminated { end_exclusive } => {
            find_import_cluster_sequence_index(source, tokens, idx + 1, end_exclusive)?;
            let err_end = unterminated_err_end(tokens, end_exclusive, import_tok.range.end);
            errors.push(crate::ParseError {
                message:
                    "syntax error: expected '.' after IMPORT FROM MEMORY ID/DATA BUFFER statement"
                        .to_string(),
                range: import_tok.range.start..err_end,
            });
            let children = token_children(b, tokens, idx, end_exclusive);
            let node = b.branch(
                SyntaxKind::Error,
                import_tok.range.start..err_end,
                &children,
            );
            Some((node, next_after_unterminated_scan(tokens, end_exclusive)))
        }
    }
}

pub fn try_parse_export_memory_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let export_tok = tokens.get(idx)?;
    if !is_keyword(source, export_tok, "export") {
        return None;
    }

    match scan_until_statement_period_with_named_args(tokens, source, idx + 1, true) {
        StmtPeriodScan::Found(period_i) => {
            let (to_idx, operand_start, operand_kind) =
                find_export_cluster_sequence_index(source, tokens, idx + 1, period_i)?;
            let mut children = vec![token_leaf(b, export_tok)];
            if let Some(from_idx) =
                find_top_level_keyword_index(source, tokens, idx + 1, to_idx, "from")
            {
                push_wrapped_expr_child(
                    b,
                    &mut children,
                    source,
                    tokens,
                    idx + 1,
                    from_idx,
                    Some(export_tok),
                    SyntaxKind::ExportMemoryNameOperand,
                );
                children.push(token_leaf(b, &tokens[from_idx]));
                push_wrapped_expr_child(
                    b,
                    &mut children,
                    source,
                    tokens,
                    from_idx + 1,
                    to_idx,
                    Some(&tokens[from_idx]),
                    SyntaxKind::ExportMemorySourceOperand,
                );
            } else {
                push_wrapped_expr_child(
                    b,
                    &mut children,
                    source,
                    tokens,
                    idx + 1,
                    to_idx,
                    Some(export_tok),
                    SyntaxKind::ExportMemorySourceOperand,
                );
            }
            push_cluster_sequence_and_operand(
                b,
                &mut children,
                source,
                tokens,
                to_idx,
                operand_start,
                period_i,
                operand_kind,
            );
            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::ExportMemoryStmt,
                export_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            Some((node, period_i + 1))
        }
        StmtPeriodScan::Unterminated { end_exclusive } => {
            find_export_cluster_sequence_index(source, tokens, idx + 1, end_exclusive)?;
            let err_end = unterminated_err_end(tokens, end_exclusive, export_tok.range.end);
            errors.push(crate::ParseError {
                message:
                    "syntax error: expected '.' after EXPORT TO MEMORY ID/DATA BUFFER statement"
                        .to_string(),
                range: export_tok.range.start..err_end,
            });
            let children = token_children(b, tokens, idx, end_exclusive);
            let node = b.branch(
                SyntaxKind::Error,
                export_tok.range.start..err_end,
                &children,
            );
            Some((node, next_after_unterminated_scan(tokens, end_exclusive)))
        }
    }
}

pub fn try_parse_update_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let update_tok = tokens.get(idx)?;
    if !is_keyword(source, update_tok, "update") {
        return None;
    }

    Some(match scan_update_stmt_period(tokens, source, idx + 1) {
        StmtPeriodScan::Found(period_i) => {
            let mut children = Vec::with_capacity(period_i - idx + 1);
            children.push(token_leaf(b, update_tok));

            let clause_start = find_top_level_clause_index(
                source,
                tokens,
                idx + 1,
                period_i,
                &["set", "from", "where", "using", "connection", "client"],
            )
            .unwrap_or(period_i);
            if let Some(target) = build_sql_data_source(b, source, tokens, idx + 1, clause_start) {
                let range = tokens[idx + 1].range.start..tokens[clause_start - 1].range.end;
                children.push(b.branch(SyntaxKind::UpdateTarget, range, &[target]));
            }

            let mut i = clause_start;
            while i < period_i {
                let token = &tokens[i];
                if is_keyword(source, token, "set") {
                    let clause_end = find_top_level_clause_index(
                        source,
                        tokens,
                        i + 1,
                        period_i,
                        &["where", "using", "connection", "client", "from"],
                    )
                    .unwrap_or(period_i);
                    let mut clause_children = vec![token_leaf(b, token)];
                    let mut assign_start = i + 1;
                    while assign_start < clause_end {
                        while assign_start < clause_end
                            && tokens[assign_start].kind == TokenKind::Comma
                        {
                            clause_children.push(token_leaf(b, &tokens[assign_start]));
                            assign_start += 1;
                        }
                        if assign_start >= clause_end {
                            break;
                        }
                        let assign_end = scan_update_set_assignment_end(
                            source,
                            tokens,
                            assign_start,
                            clause_end,
                        );
                        let Some(eq_idx) = find_top_level_token_kind(
                            tokens,
                            assign_start,
                            assign_end,
                            TokenKind::Eq,
                        )
                        .or_else(|| {
                            find_top_level_token_kind(
                                tokens,
                                assign_start,
                                assign_end,
                                TokenKind::QuestionEq,
                            )
                        }) else {
                            push_token_children(
                                b,
                                &mut clause_children,
                                tokens,
                                assign_start,
                                assign_end,
                            );
                            assign_start = assign_end;
                            continue;
                        };
                        let mut assignment_children = Vec::new();
                        push_token_children(
                            b,
                            &mut assignment_children,
                            tokens,
                            assign_start,
                            eq_idx + 1,
                        );
                        if eq_idx + 1 < assign_end {
                            push_wrapped_expr_child(
                                b,
                                &mut assignment_children,
                                source,
                                tokens,
                                eq_idx + 1,
                                assign_end,
                                Some(&tokens[eq_idx]),
                                SyntaxKind::UpdateSetValueOperand,
                            );
                        }
                        let assignment = b.branch(
                            SyntaxKind::UpdateSetAssignment,
                            tokens[assign_start].range.start..tokens[assign_end - 1].range.end,
                            &assignment_children,
                        );
                        clause_children.push(assignment);
                        assign_start = assign_end;
                    }
                    let clause = b.branch(
                        SyntaxKind::UpdateSetClause,
                        token.range.start..tokens[clause_end.saturating_sub(1)].range.end,
                        &clause_children,
                    );
                    children.push(clause);
                    i = clause_end;
                    continue;
                }
                if is_keyword(source, token, "from") {
                    children.push(token_leaf(b, token));
                    let from_end = find_top_level_clause_index(
                        source,
                        tokens,
                        i + 1,
                        period_i,
                        &["where", "using", "connection", "client"],
                    )
                    .unwrap_or(period_i);
                    if i + 1 < from_end {
                        push_wrapped_expr_child(
                            b,
                            &mut children,
                            source,
                            tokens,
                            i + 1,
                            from_end,
                            Some(token),
                            SyntaxKind::UpdateFromOperand,
                        );
                    }
                    i = from_end;
                    continue;
                }
                if is_keyword(source, token, "where") {
                    let mut clause_children = vec![token_leaf(b, token)];
                    let predicate_start = skip_trivia(tokens, i + 1);
                    if predicate_start < period_i
                        && tokens.get(predicate_start).map(|token| token.kind)
                            == Some(TokenKind::LParen)
                        && let Some(dynamic_end) = find_matching_delim(
                            tokens,
                            predicate_start,
                            TokenKind::LParen,
                            TokenKind::RParen,
                        )
                        && dynamic_end + 1 == period_i
                        && let Some(dynamic_node) = build_token_branch(
                            b,
                            SyntaxKind::SqlDynamicWhere,
                            tokens,
                            predicate_start,
                            dynamic_end + 1,
                        )
                    {
                        clause_children.push(dynamic_node);
                    } else if i + 1 < period_i {
                        push_logical_expr_child(
                            b,
                            &mut clause_children,
                            source,
                            tokens,
                            i + 1,
                            period_i,
                            Some(token),
                        );
                    }
                    let clause = b.branch(
                        SyntaxKind::UpdateWhereClause,
                        token.range.start..tokens[period_i - 1].range.end,
                        &clause_children,
                    );
                    children.push(clause);
                    i = period_i;
                    continue;
                }
                children.push(token_leaf(b, token));
                i += 1;
            }
            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::UpdateStmt,
                update_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        }
        StmtPeriodScan::Unterminated { end_exclusive } => {
            let err_end = unterminated_err_end(tokens, end_exclusive, update_tok.range.end);
            errors.push(crate::ParseError {
                message: "syntax error: expected '.' after UPDATE statement".to_string(),
                range: update_tok.range.start..err_end,
            });
            let children = token_children(b, tokens, idx, end_exclusive);
            let node = b.branch(
                SyntaxKind::Error,
                update_tok.range.start..err_end,
                &children,
            );
            (node, next_after_unterminated_scan(tokens, end_exclusive))
        }
    })
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

    Some(parse_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        idx + 1,
        assign_tok,
        "syntax error: expected '.' after ASSIGN statement",
        errors,
        |_, end_exclusive| end_exclusive,
        |b, period_i, _errors| {
            let mut children = Vec::with_capacity(period_i - idx + 1);
            children.push(token_leaf(b, assign_tok));
            let Some(to_idx) =
                find_top_level_keyword_index(source, tokens, idx + 1, period_i, "to")
            else {
                let raw = token_children(b, tokens, idx, period_i + 1);
                let node = b.branch(
                    SyntaxKind::AssignKeywordStmt,
                    assign_tok.range.start..tokens[period_i].range.end,
                    &raw,
                );
                return (node, period_i + 1);
            };

            if tokens
                .get(idx + 1)
                .is_some_and(|token| is_keyword(source, token, "component"))
            {
                children.push(token_leaf(b, &tokens[idx + 1]));
                if let Some(of_idx) =
                    find_top_level_keyword_index(source, tokens, idx + 2, to_idx, "of")
                {
                    push_expr_child(
                        b,
                        &mut children,
                        source,
                        tokens,
                        idx + 2,
                        of_idx,
                        Some(&tokens[idx + 1]),
                    );
                    children.push(token_leaf(b, &tokens[of_idx]));
                    if tokens
                        .get(of_idx + 1)
                        .is_some_and(|token| is_keyword(source, token, "structure"))
                    {
                        children.push(token_leaf(b, &tokens[of_idx + 1]));
                        let source_expr = parse_arithmetic_expr(
                            b,
                            source,
                            &tokens[of_idx + 2..to_idx],
                            Some(&tokens[of_idx + 1]),
                        );
                        children.push(b.branch(
                            SyntaxKind::AssignSourceExpr,
                            tokens[of_idx + 2].range.start..tokens[to_idx - 1].range.end,
                            &[source_expr],
                        ));
                    }
                } else {
                    let source_expr = parse_arithmetic_expr(
                        b,
                        source,
                        &tokens[idx + 2..to_idx],
                        Some(&tokens[idx + 1]),
                    );
                    children.push(b.branch(
                        SyntaxKind::AssignSourceExpr,
                        tokens[idx + 2].range.start..tokens[to_idx - 1].range.end,
                        &[source_expr],
                    ));
                }
            } else {
                let source_expr =
                    parse_arithmetic_expr(b, source, &tokens[idx + 1..to_idx], Some(assign_tok));
                children.push(b.branch(
                    SyntaxKind::AssignSourceExpr,
                    tokens[idx + 1].range.start..tokens[to_idx - 1].range.end,
                    &[source_expr],
                ));
            }

            let casting_idx =
                find_top_level_keyword_index(source, tokens, to_idx + 1, period_i, "casting");
            let target_end = casting_idx.unwrap_or(period_i);

            children.push(token_leaf(b, &tokens[to_idx]));
            if let Some((inline_decl, next_i)) =
                try_parse_field_symbol_inline_decl(b, source, tokens, to_idx + 1)
                && skip_trivia(tokens, next_i) == target_end
            {
                children.push(inline_decl);
            } else {
                push_expr_child(
                    b,
                    &mut children,
                    source,
                    tokens,
                    to_idx + 1,
                    target_end,
                    Some(&tokens[to_idx]),
                );
            }

            if let Some(casting_idx) = casting_idx {
                children.push(token_leaf(b, &tokens[casting_idx]));
                let clause_idx = casting_idx + 1;
                if clause_idx < period_i {
                    let clause_token = &tokens[clause_idx];
                    if is_keyword(source, clause_token, "type") {
                        children.push(token_leaf(b, clause_token));
                        let type_start = clause_idx + 1;
                        if type_start < period_i {
                            if tokens[type_start].kind == TokenKind::LParen {
                                push_expr_child(
                                    b,
                                    &mut children,
                                    source,
                                    tokens,
                                    type_start,
                                    period_i,
                                    Some(clause_token),
                                );
                            } else if let Some((type_ref, next_i)) =
                                parse_type_ref_tokens(b, source, tokens, type_start, &[])
                            {
                                children.push(type_ref);
                                for token in &tokens[next_i..period_i] {
                                    children.push(token_leaf(b, token));
                                }
                            }
                        }
                    } else if is_keyword(source, clause_token, "like") {
                        children.push(token_leaf(b, clause_token));
                        push_expr_child(
                            b,
                            &mut children,
                            source,
                            tokens,
                            clause_idx + 1,
                            period_i,
                            Some(clause_token),
                        );
                    } else {
                        for token in &tokens[clause_idx..period_i] {
                            children.push(token_leaf(b, token));
                        }
                    }
                }
            }
            children.push(token_leaf(b, &tokens[period_i]));
            let node = b.branch(
                SyntaxKind::AssignKeywordStmt,
                assign_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        },
    ))
}

pub fn try_parse_event_block(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let start_tok = tokens.get(idx)?;
    let body_start_idx = event_block_header_end(source, tokens, idx)?;

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
        EVENT_BLOCK_BODY_BOUNDARY_KEYWORDS,
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

pub fn try_parse_macro_def(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let start_tok = tokens.get(idx)?;
    if !is_keyword(source, start_tok, "define") {
        return None;
    }

    let (mut children, next) = parse_header_until_period(
        b,
        source,
        tokens,
        idx,
        idx + 1,
        errors,
        "syntax error: expected '.' after DEFINE header",
    );

    let Some(end_idx) = find_macro_end_keyword(source, tokens, next) else {
        let eof_idx = tokens
            .iter()
            .position(|token| token.kind == TokenKind::Eof)
            .unwrap_or(tokens.len());
        let err_end = unterminated_err_end(tokens, eof_idx, start_tok.range.end);
        errors.push(crate::ParseError {
            message: "syntax error: expected END-OF-DEFINITION".to_string(),
            range: start_tok.range.start..err_end,
        });
        children.extend(token_children(b, tokens, next, eof_idx));
        let node = b.branch(
            SyntaxKind::MacroDef,
            start_tok.range.start..err_end,
            &children,
        );
        return Some((node, next_after_unterminated_scan(tokens, eof_idx)));
    };

    children.extend(token_children(b, tokens, next, end_idx));
    let end_parts_end = macro_end_keyword_end(source, tokens, end_idx).unwrap_or(end_idx + 1);
    let period_idx = skip_trivia(tokens, end_parts_end);
    if let Some(period_tok) = tokens.get(period_idx)
        && period_tok.kind == TokenKind::Period
    {
        children.extend(token_children(b, tokens, end_idx, period_idx + 1));
        let node = b.branch(
            SyntaxKind::MacroDef,
            start_tok.range.start..period_tok.range.end,
            &children,
        );
        return Some((node, period_idx + 1));
    }

    let end_tok = &tokens[end_parts_end.saturating_sub(1)];
    let err_end = tokens
        .get(period_idx)
        .map(|token| token.range.end)
        .unwrap_or(end_tok.range.end);
    errors.push(crate::ParseError {
        message: "syntax error: expected '.' after END-OF-DEFINITION".to_string(),
        range: end_tok.range.start..err_end,
    });
    children.extend(token_children(b, tokens, end_idx, end_parts_end));
    let node = b.branch(
        SyntaxKind::MacroDef,
        start_tok.range.start..err_end,
        &children,
    );
    let next = if tokens.get(period_idx).map(|token| token.kind) == Some(TokenKind::Eof) {
        tokens.len()
    } else {
        end_parts_end
    };
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

pub fn try_parse_enhancement_point_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let lead_end = match_hyphenated_keyword(source, tokens, idx, &["enhancement", "point"])?;
    let start_tok = tokens.get(idx)?;
    Some(parse_stmt_with_period_scan(
        b,
        source,
        tokens,
        idx,
        lead_end,
        start_tok,
        "syntax error: expected '.' after ENHANCEMENT-POINT statement",
        errors,
        next_after_unterminated_scan,
        |b, period_i, _errors| {
            let children = token_children(b, tokens, idx, period_i + 1);
            let node = b.branch(
                SyntaxKind::EnhancementPointStmt,
                start_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        },
    ))
}

pub fn try_parse_enhancement_section_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let lead_end = match_hyphenated_keyword(source, tokens, idx, &["enhancement", "section"])?;
    let start_tok = tokens.get(idx)?;
    let (mut children, next) = parse_header_until_period(
        b,
        source,
        tokens,
        idx,
        lead_end,
        errors,
        "syntax error: expected '.' after ENHANCEMENT-SECTION header",
    );
    let (body, after_body) = parse_body_until_enhancement_end(
        b,
        source,
        tokens,
        next,
        errors,
        EnhancementEndKind::Section,
    );
    children.extend(body);
    let (end_children, next_after, end_pos) = parse_enhancement_end_keyword(
        b,
        source,
        tokens,
        after_body,
        start_tok,
        EnhancementEndKind::Section,
        errors,
    );
    children.extend(end_children);
    let node = b.branch(
        SyntaxKind::EnhancementSectionStmt,
        start_tok.range.start..end_pos,
        &children,
    );
    Some((node, next_after))
}

pub fn try_parse_enhancement_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    if starts_hyphenated_keyword(tokens, idx) {
        return None;
    }
    let start_tok = tokens.get(idx)?;
    if !is_keyword(source, start_tok, "enhancement") {
        return None;
    }
    let (mut children, next) = parse_header_until_period(
        b,
        source,
        tokens,
        idx,
        idx + 1,
        errors,
        "syntax error: expected '.' after ENHANCEMENT header",
    );
    let (body, after_body) = parse_body_until_enhancement_end(
        b,
        source,
        tokens,
        next,
        errors,
        EnhancementEndKind::Enhancement,
    );
    children.extend(body);
    let (end_children, next_after, end_pos) = parse_enhancement_end_keyword(
        b,
        source,
        tokens,
        after_body,
        start_tok,
        EnhancementEndKind::Enhancement,
        errors,
    );
    children.extend(end_children);
    let node = b.branch(
        SyntaxKind::EnhancementStmt,
        start_tok.range.start..end_pos,
        &children,
    );
    Some((node, next_after))
}

pub fn try_parse_function_decl(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    if starts_hyphenated_keyword(tokens, idx) {
        return None;
    }
    let start_tok = tokens.get(idx)?;
    if !is_keyword(source, start_tok, "function") {
        return None;
    }

    let mut next = idx + 1;
    while matches!(tokens.get(next), Some(token) if token.kind == TokenKind::Comment) {
        next += 1;
    }
    if matches!(
        tokens.get(next).map(|token| token.kind),
        Some(TokenKind::Eq | TokenKind::QuestionEq)
    ) {
        return None;
    }
    let Some(name_tok) = tokens.get(next) else {
        let start_leaf = token_leaf(b, start_tok);
        let node = b.branch(
            SyntaxKind::FunctionDecl,
            start_tok.range.clone(),
            &[start_leaf],
        );
        return Some((node, next));
    };
    if name_tok.kind != TokenKind::Ident {
        let start_leaf = token_leaf(b, start_tok);
        let name_leaf = token_leaf(b, name_tok);
        errors.push(crate::ParseError {
            message: "syntax error: expected function module name after FUNCTION".to_string(),
            range: start_tok.range.start..name_tok.range.end,
        });
        return Some((
            b.branch(
                SyntaxKind::FunctionDecl,
                start_tok.range.start..name_tok.range.end,
                &[start_leaf, name_leaf],
            ),
            next + 1,
        ));
    }

    next += 1;
    while matches!(tokens.get(next), Some(token) if token.kind == TokenKind::Comment) {
        next += 1;
    }

    let (children, next, header_end) = if tokens.get(next).map(|token| token.kind)
        == Some(TokenKind::Period)
    {
        (
            build_function_header_children(b, source, tokens, idx, next),
            next + 1,
            tokens[next].range.end,
        )
    } else if tokens
        .get(next)
        .is_some_and(|token| function_header_section_keyword(source, token))
    {
        match scan_until_top_level_period(tokens, next) {
            Some(period_i) => (
                build_function_header_children(b, source, tokens, idx, period_i),
                period_i + 1,
                tokens[period_i].range.end,
            ),
            None => {
                let end_exclusive = tokens
                    .iter()
                    .position(|token| token.kind == TokenKind::Eof)
                    .unwrap_or(tokens.len());
                let err_end = unterminated_err_end(tokens, end_exclusive, name_tok.range.end);
                errors.push(crate::ParseError {
                    message: "syntax error: expected '.' after function header".to_string(),
                    range: start_tok.range.start..err_end,
                });
                let mut children = build_function_header_children(b, source, tokens, idx, next - 1);
                let err_children = error_token_children(b, tokens, next, end_exclusive);
                children.push(b.branch(
                    SyntaxKind::Error,
                    tokens[next].range.start..err_end,
                    &err_children,
                ));
                (
                    children,
                    next_after_unterminated_scan(tokens, end_exclusive),
                    err_end,
                )
            }
        }
    } else {
        (
            build_function_header_children(b, source, tokens, idx, next - 1),
            next,
            name_tok.range.end,
        )
    };

    let (body, after_body) =
        parse_body_until_keywords(b, source, tokens, next, errors, &["ENDFUNCTION"]);
    let mut children = children;
    children.extend(body);
    let (end_children, next_after, end_pos) = parse_end_keyword(
        b,
        source,
        tokens,
        after_body,
        start_tok,
        "ENDFUNCTION",
        "syntax error: expected ENDFUNCTION",
        errors,
    );
    children.extend(end_children);
    let node = b.branch(
        SyntaxKind::FunctionDecl,
        start_tok.range.start..end_pos.max(header_end),
        &children,
    );
    Some((node, next_after))
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
    let start_tok = tokens.get(idx)?;
    if !is_keyword(source, start_tok, "class") {
        return None;
    }
    let (mut children, mut next) = match scan_until_statement_period(tokens, source, idx + 1) {
        StmtPeriodScan::Found(period_i) => (
            build_class_header_children(b, source, tokens, idx, period_i),
            period_i + 1,
        ),
        StmtPeriodScan::Unterminated { end_exclusive } => {
            let err_end = unterminated_err_end(tokens, end_exclusive, start_tok.range.end);
            errors.push(crate::ParseError {
                message: "syntax error: expected '.' after class header".to_string(),
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
        parse_body_until_keywords(b, source, tokens, next, errors, &["ENDCLASS"]);
    children.extend(body);
    next = after_body;
    let (end_children, next_after, end_pos) = parse_end_keyword(
        b,
        source,
        tokens,
        next,
        start_tok,
        "ENDCLASS",
        "syntax error: expected ENDCLASS",
        errors,
    );
    children.extend(end_children);
    let node = b.branch(
        SyntaxKind::ClassDecl,
        start_tok.range.start..end_pos,
        &children,
    );
    Some((node, next_after))
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
    if !interface_header_is_block(tokens, source, idx) {
        return None;
    }
    let start_tok = tokens.get(idx)?;
    if !is_keyword(source, start_tok, "interface") {
        return None;
    }
    let (mut children, mut next) = match scan_until_statement_period(tokens, source, idx + 1) {
        StmtPeriodScan::Found(period_i) => (
            build_interface_header_children(b, tokens, idx, period_i),
            period_i + 1,
        ),
        StmtPeriodScan::Unterminated { end_exclusive } => {
            let err_end = unterminated_err_end(tokens, end_exclusive, start_tok.range.end);
            errors.push(crate::ParseError {
                message: "syntax error: expected '.' after interface header".to_string(),
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
        parse_body_until_keywords(b, source, tokens, next, errors, &["ENDINTERFACE"]);
    children.extend(body);
    next = after_body;
    let (end_children, next_after, end_pos) = parse_end_keyword(
        b,
        source,
        tokens,
        next,
        start_tok,
        "ENDINTERFACE",
        "syntax error: expected ENDINTERFACE",
        errors,
    );
    children.extend(end_children);
    let node = b.branch(
        SyntaxKind::InterfaceDecl,
        start_tok.range.start..end_pos,
        &children,
    );
    Some((node, next_after))
}

pub fn try_parse_method_decl(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let start_tok = tokens.get(idx)?;
    if !is_keyword(source, start_tok, "method") {
        return None;
    }
    let (mut children, mut next, is_amdp) =
        match scan_until_statement_period(tokens, source, idx + 1) {
            StmtPeriodScan::Found(period_i) => (
                build_method_header_children(b, source, tokens, idx, period_i),
                period_i + 1,
                method_header_is_amdp(source, tokens, idx, period_i),
            ),
            StmtPeriodScan::Unterminated { end_exclusive } => {
                let err_end = unterminated_err_end(tokens, end_exclusive, start_tok.range.end);
                errors.push(crate::ParseError {
                    message: "syntax error: expected '.' after method header".to_string(),
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
                    false,
                )
            }
        };
    let (body, after_body) = if is_amdp {
        parse_sqlscript_island_until_endmethod(b, source, tokens, next)
    } else {
        parse_body_until_keywords(b, source, tokens, next, errors, &["ENDMETHOD"])
    };
    children.extend(body);
    next = after_body;
    let (end_children, next_after, end_pos) = parse_end_keyword(
        b,
        source,
        tokens,
        next,
        start_tok,
        "ENDMETHOD",
        "syntax error: expected ENDMETHOD",
        errors,
    );
    children.extend(end_children);
    let node = b.branch(
        SyntaxKind::MethodDecl,
        start_tok.range.start..end_pos,
        &children,
    );
    Some((node, next_after))
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

    let (mut children, next) = parse_select_header_until_period(b, source, tokens, idx, errors);

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

pub fn try_parse_open_cursor_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let open_tok = tokens.get(idx)?;
    if !is_keyword(source, open_tok, "open") {
        return None;
    }

    let cursor_idx = skip_trivia(tokens, idx + 1);
    let cursor_tok = tokens.get(cursor_idx)?;
    if !is_keyword(source, cursor_tok, "cursor") {
        return None;
    }

    let mut handle_start = skip_trivia(tokens, cursor_idx + 1);
    if let Some(with_hold_end) =
        match_keyword_sequence(source, tokens, handle_start, &["with", "hold"])
    {
        handle_start = skip_trivia(tokens, with_hold_end);
    }

    let Some(period_i) = scan_until_top_level_period(tokens, cursor_idx + 1) else {
        let eof_idx = tokens
            .iter()
            .position(|token| token.kind == TokenKind::Eof)
            .unwrap_or(tokens.len());
        let err_end = tokens
            .get(eof_idx.saturating_sub(1))
            .map(|token| token.range.end)
            .unwrap_or(open_tok.range.end);
        errors.push(crate::ParseError {
            message: "syntax error: expected '.' after OPEN CURSOR statement".to_string(),
            range: open_tok.range.start..err_end,
        });
        let err_children = error_token_children(b, tokens, idx, eof_idx);
        let node = b.branch(
            SyntaxKind::Error,
            open_tok.range.start..err_end,
            &err_children,
        );
        return Some((node, tokens.len()));
    };

    let Some(for_idx) = find_top_level_keyword(source, tokens, handle_start, period_i, "for")
    else {
        return None;
    };
    let select_idx = skip_trivia(tokens, for_idx + 1);
    if !tokens
        .get(select_idx)
        .is_some_and(|token| is_keyword(source, token, "select"))
    {
        return None;
    }

    let mut children = Vec::new();
    push_token_children(b, &mut children, tokens, idx, handle_start);
    if let Some(handle) = build_token_branch(
        b,
        SyntaxKind::CursorHandleOperand,
        tokens,
        handle_start,
        for_idx,
    ) {
        children.push(handle);
    }
    push_token_children(b, &mut children, tokens, for_idx, select_idx + 1);
    let (select_children, next) =
        parse_select_header_until_period(b, source, tokens, select_idx, errors);
    children.extend(select_children);

    let end = children
        .last()
        .copied()
        .map(|id| b.span(id).end)
        .unwrap_or(open_tok.range.end);
    let node = b.branch(
        SyntaxKind::OpenCursorStmt,
        open_tok.range.start..end,
        &children,
    );
    Some((node, next))
}

fn fetch_cursor_tail_clause_kind(
    source: &str,
    tokens: &[Token],
    idx: usize,
) -> Option<SelectClauseKind> {
    let kind = select_clause_start_kind(source, tokens, idx)?;
    match kind {
        SelectClauseKind::Into | SelectClauseKind::Appending | SelectClauseKind::PackageSize => {
            Some(kind)
        }
        _ => None,
    }
}

pub fn try_parse_fetch_cursor_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let fetch_tok = tokens.get(idx)?;
    if !is_keyword(source, fetch_tok, "fetch") {
        return None;
    }

    let next_idx = skip_trivia(tokens, idx + 1);
    let next_tok = tokens.get(next_idx)?;
    if !is_keyword(source, next_tok, "next") {
        return None;
    }

    let cursor_idx = skip_trivia(tokens, next_idx + 1);
    let cursor_tok = tokens.get(cursor_idx)?;
    if !is_keyword(source, cursor_tok, "cursor") {
        return None;
    }

    let Some(period_i) = scan_until_top_level_period(tokens, cursor_idx + 1) else {
        let eof_idx = tokens
            .iter()
            .position(|token| token.kind == TokenKind::Eof)
            .unwrap_or(tokens.len());
        let err_end = tokens
            .get(eof_idx.saturating_sub(1))
            .map(|token| token.range.end)
            .unwrap_or(fetch_tok.range.end);
        errors.push(crate::ParseError {
            message: "syntax error: expected '.' after FETCH NEXT CURSOR statement".to_string(),
            range: fetch_tok.range.start..err_end,
        });
        let err_children = error_token_children(b, tokens, idx, eof_idx);
        let node = b.branch(
            SyntaxKind::Error,
            fetch_tok.range.start..err_end,
            &err_children,
        );
        return Some((node, tokens.len()));
    };

    let handle_start = skip_trivia(tokens, cursor_idx + 1);
    let tail_start = find_top_level_keyword_in(
        source,
        tokens,
        handle_start,
        period_i,
        &["into", "appending"],
    )?;
    if handle_start >= tail_start {
        return None;
    }

    let mut children = Vec::new();
    push_token_children(b, &mut children, tokens, idx, handle_start);
    if let Some(handle) = build_token_branch(
        b,
        SyntaxKind::CursorHandleOperand,
        tokens,
        handle_start,
        tail_start,
    ) {
        children.push(handle);
    }

    let mut cursor = tail_start;
    while cursor < period_i {
        if let Some(kind) = fetch_cursor_tail_clause_kind(source, tokens, cursor) {
            let clause_end = scan_until_clause(tokens, cursor + 1, period_i, |tokens, idx| {
                fetch_cursor_tail_clause_kind(source, tokens, idx).is_some()
            });
            if let Some(clause) = build_select_clause(b, source, tokens, kind, cursor, clause_end) {
                children.push(clause);
            }
            cursor = clause_end;
        } else {
            let next_clause = scan_until_clause(tokens, cursor + 1, period_i, |tokens, idx| {
                fetch_cursor_tail_clause_kind(source, tokens, idx).is_some()
            });
            push_token_children(b, &mut children, tokens, cursor, next_clause);
            cursor = next_clause;
        }
    }
    push_token_children(b, &mut children, tokens, period_i, period_i + 1);

    let end = children
        .last()
        .copied()
        .map(|id| b.span(id).end)
        .unwrap_or(fetch_tok.range.end);
    let node = b.branch(
        SyntaxKind::FetchCursorStmt,
        fetch_tok.range.start..end,
        &children,
    );
    Some((node, period_i + 1))
}

pub fn try_parse_close_cursor_stmt(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let close_tok = tokens.get(idx)?;
    if !is_keyword(source, close_tok, "close") {
        return None;
    }

    let cursor_idx = skip_trivia(tokens, idx + 1);
    let cursor_tok = tokens.get(cursor_idx)?;
    if !is_keyword(source, cursor_tok, "cursor") {
        return None;
    }

    let Some(period_i) = scan_until_top_level_period(tokens, cursor_idx + 1) else {
        let eof_idx = tokens
            .iter()
            .position(|token| token.kind == TokenKind::Eof)
            .unwrap_or(tokens.len());
        let err_end = tokens
            .get(eof_idx.saturating_sub(1))
            .map(|token| token.range.end)
            .unwrap_or(close_tok.range.end);
        errors.push(crate::ParseError {
            message: "syntax error: expected '.' after CLOSE CURSOR statement".to_string(),
            range: close_tok.range.start..err_end,
        });
        let err_children = error_token_children(b, tokens, idx, eof_idx);
        let node = b.branch(
            SyntaxKind::Error,
            close_tok.range.start..err_end,
            &err_children,
        );
        return Some((node, tokens.len()));
    };

    let handle_start = skip_trivia(tokens, cursor_idx + 1);
    if handle_start >= period_i {
        return None;
    }

    let mut children = Vec::new();
    push_token_children(b, &mut children, tokens, idx, handle_start);
    if let Some(handle) = build_token_branch(
        b,
        SyntaxKind::CursorHandleOperand,
        tokens,
        handle_start,
        period_i,
    ) {
        children.push(handle);
    }
    push_token_children(b, &mut children, tokens, period_i, period_i + 1);

    let end = children
        .last()
        .copied()
        .map(|id| b.span(id).end)
        .unwrap_or(close_tok.range.end);
    let node = b.branch(
        SyntaxKind::CloseCursorStmt,
        close_tok.range.start..end,
        &children,
    );
    Some((node, period_i + 1))
}

#[cfg(test)]
mod tests {
    use abap_ast::SyntaxKind;
    use abap_ast::ast::{
        AstNode, CallStmt, CallStmtKind, ClassDecl, CloseCursorStmt, DataLikeDecl,
        DataLikeStorageKind, FetchCursorStmt, FormDecl, FormParamPassingKind, FormParamSectionKind,
        FunctionDecl, FunctionParamSectionKind, IncludeStmt, MethodDecl, OpenCursorStmt,
        SelectIntoClause, SelectStmt, SubmitStmt, SyntaxNodeRef, WriteStmt,
    };

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
    fn parses_enhancement_point_statement() {
        let parsed = crate::parse(
            "METHOD run.\n\
               ENHANCEMENT-POINT z_ep SPOTS es_demo STATIC INCLUDE BOUND.\n\
             ENDMETHOD.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::MethodDecl), 1);
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::EnhancementPointStmt),
            1
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_enhancement_section_body_until_matching_end() {
        let parsed = crate::parse(
            "METHOD run.\n\
               ENHANCEMENT-SECTION z_sec SPOTS es_demo INCLUDE BOUND.\n\
                 IF lv_ok = abap_true.\n\
                   WRITE 'x'.\n\
                 ENDIF.\n\
               END-ENHANCEMENT-SECTION.\n\
               WRITE 'y'.\n\
             ENDMETHOD.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::EnhancementSectionStmt),
            1
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::IfStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::WriteStmt), 2);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_enhancement_implementation_block() {
        let parsed = crate::parse(
            "ENHANCEMENT 1 z_impl SPOTS es_demo.\n\
               WRITE 'x'.\n\
             ENDENHANCEMENT.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::EnhancementStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::WriteStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn stray_enhancement_ends_recover_at_statement_boundary() {
        let parsed = crate::parse(
            "END-ENHANCEMENT-SECTION.\n\
             ENDENHANCEMENT.\n\
             WRITE 'x'.",
        );
        let messages = parsed
            .errors
            .iter()
            .map(|error| error.message.as_str())
            .collect::<Vec<_>>();
        assert_eq!(
            messages,
            vec![
                "syntax error: unexpected END-ENHANCEMENT-SECTION without matching ENHANCEMENT-SECTION",
                "syntax error: unexpected ENDENHANCEMENT without matching ENHANCEMENT",
            ]
        );
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::WriteStmt), 1);
    }

    #[test]
    fn parses_all_supported_event_block_leads() {
        let parsed = crate::parse(
            "INITIALIZATION.\nWRITE 'a'.\n\
START-OF-SELECTION.\nWRITE 'b'.\n\
END-OF-SELECTION.\nWRITE 'c'.\n\
TOP-OF-PAGE.\nWRITE 'd'.\n\
END-OF-PAGE.\nWRITE 'e'.\n\
AT LINE-SELECTION.\nWRITE 'f'.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::EventBlock), 6);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::WriteStmt), 6);
    }

    #[test]
    fn parses_at_selection_screen_event_block_variants() {
        let parsed = crate::parse(
            "AT SELECTION-SCREEN OUTPUT.\nWRITE 'a'.\n\
AT SELECTION-SCREEN.\nWRITE 'b'.\n\
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_pub.\nWRITE 'c'.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::EventBlock), 3);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::WriteStmt), 3);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
    }

    #[test]
    fn classifies_raise_event_statement_specifically() {
        let parsed = crate::parse(
            "CLASS lcl_demo IMPLEMENTATION.\n  METHOD trigger.\n    RAISE EVENT changed EXPORTING value = 'x'.\n  ENDMETHOD.\nENDCLASS.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::RaiseEventStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::CallArgList), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
    }

    #[test]
    fn parses_selection_screen_block_statements_structurally() {
        let parsed = crate::parse(
            "SELECTION-SCREEN BEGIN OF BLOCK date WITH FRAME TITLE gv_fselc.\n\
SELECTION-SCREEN END OF BLOCK date.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::SelectionScreenStmt),
            2
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
    }

    #[test]
    fn parses_chained_selection_screen_block_with_comments() {
        let parsed = crate::parse(
            "SELECTION-SCREEN: BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-b02,\n\
COMMENT /1(79) TEXT-003,\n\
COMMENT /1(79) TEXT-004,\n\
COMMENT /1(79) TEXT-005,\n\
COMMENT /1(79) TEXT-999,\n\
COMMENT /1(79) TEXT-006,\n\
COMMENT /1(79) TEXT-007,\n\
COMMENT /1(79) TEXT-008,\n\
END OF BLOCK b02.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::SelectionScreenStmt),
            1
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
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
    fn form_header_exposes_structured_sections_and_params() {
        let src = "FORM run TABLES it_tab USING VALUE(iv_row) TYPE REF TO zif_demo=>ty_row CHANGING cv_text LIKE lv_text. ENDFORM.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let form = FormDecl::cast(SyntaxNodeRef::new(
            &parsed.file,
            parsed
                .file
                .find_first_kind(parsed.file.root(), SyntaxKind::FormDecl)
                .expect("form decl"),
        ))
        .expect("form decl");
        assert_eq!(
            form.name_token().and_then(|name| name.name(src)).as_deref(),
            Some("run")
        );
        let sections = form.param_sections().collect::<Vec<_>>();
        assert_eq!(sections.len(), 3);
        assert_eq!(sections[0].kind(src), Some(FormParamSectionKind::Tables));
        assert_eq!(sections[1].kind(src), Some(FormParamSectionKind::Using));
        assert_eq!(sections[2].kind(src), Some(FormParamSectionKind::Changing));
        let using_param = sections[1].params().next().expect("using param");
        assert_eq!(
            using_param
                .name_token()
                .and_then(|name| name.name(src))
                .as_deref(),
            Some("iv_row")
        );
        assert_eq!(using_param.passing_kind(src), FormParamPassingKind::Value);
        assert_eq!(
            parsed
                .file
                .count_kind(form.syntax().id(), SyntaxKind::FormParam),
            3
        );
        assert_eq!(
            parsed
                .file
                .count_kind(form.syntax().id(), SyntaxKind::FormParamSection),
            3
        );
    }

    #[test]
    fn parses_form_header_with_multiline_tables_using_changing_sections() {
        let src = "FORM run\n  TABLES it_tab\n  USING iv_row\n  CHANGING cv_text.\nENDFORM.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::FormDecl), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn form_header_exposes_raising_section_and_exception_type_refs() {
        let src = "FORM run RAISING resumable(/sttp/cx_demo) cx_other. ENDFORM.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let form = FormDecl::cast(SyntaxNodeRef::new(
            &parsed.file,
            parsed
                .file
                .find_first_kind(parsed.file.root(), SyntaxKind::FormDecl)
                .expect("form decl"),
        ))
        .expect("form decl");
        let sections = form.param_sections().collect::<Vec<_>>();
        assert_eq!(sections.len(), 1);
        assert_eq!(sections[0].kind(src), Some(FormParamSectionKind::Raising));
        let entries = sections[0].params().collect::<Vec<_>>();
        assert_eq!(entries.len(), 2);
        assert!(entries[0].is_resumable(src));
        assert_eq!(
            entries[0]
                .type_ref()
                .and_then(|type_ref| type_ref.display_text(src)),
            Some("/sttp/cx_demo")
        );
        assert_eq!(
            entries[1]
                .type_ref()
                .and_then(|type_ref| type_ref.display_text(src)),
            Some("cx_other")
        );
    }

    #[test]
    fn include_stmt_exposes_structured_names() {
        let src = "INCLUDE: lfoo, lbar.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let include = IncludeStmt::cast(SyntaxNodeRef::new(
            &parsed.file,
            parsed
                .file
                .find_first_kind(parsed.file.root(), SyntaxKind::IncludeStmt)
                .expect("include stmt"),
        ))
        .expect("include stmt");
        let names = include
            .names()
            .filter_map(|name| name.name(src))
            .collect::<Vec<_>>();
        assert_eq!(
            names.iter().map(|name| name.as_ref()).collect::<Vec<_>>(),
            vec!["lfoo", "lbar"]
        );
        assert_eq!(
            parsed
                .file
                .count_kind(include.syntax().id(), SyntaxKind::IncludeName),
            2
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
    fn class_decl_exposes_name_superclass_and_implementation_marker() {
        let src = "CLASS lcl_demo DEFINITION ABSTRACT INHERITING FROM zcl_base. ENDCLASS. CLASS lcl_demo IMPLEMENTATION. ENDCLASS.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let class_nodes = parsed
            .file
            .children(parsed.file.root())
            .filter(|&node| parsed.file.kind(node) == SyntaxKind::ClassDecl)
            .collect::<Vec<_>>();
        let def = ClassDecl::cast(SyntaxNodeRef::new(&parsed.file, class_nodes[0])).expect("def");
        let imp = ClassDecl::cast(SyntaxNodeRef::new(&parsed.file, class_nodes[1])).expect("impl");
        assert_eq!(
            def.name_token().and_then(|name| name.name(src)).as_deref(),
            Some("lcl_demo")
        );
        assert_eq!(
            def.superclass()
                .and_then(|type_ref| type_ref.display_text(src))
                .map(str::to_ascii_lowercase)
                .as_deref(),
            Some("zcl_base")
        );
        assert!(def.is_abstract(src));
        assert!(!def.is_implementation());
        assert!(!imp.is_abstract(src));
        assert!(imp.is_implementation());
    }

    #[test]
    fn method_decl_target_exposes_interface_qualifier() {
        let src = "CLASS lcl IMPLEMENTATION. METHOD if_demo~run. ENDMETHOD. ENDCLASS.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let method = MethodDecl::cast(SyntaxNodeRef::new(
            &parsed.file,
            parsed
                .file
                .find_first_kind(parsed.file.root(), SyntaxKind::MethodDecl)
                .expect("method"),
        ))
        .expect("method");
        let target = method.target().expect("target");
        assert_eq!(
            target
                .qualifier()
                .and_then(|type_ref| type_ref.display_text(src))
                .map(str::to_ascii_lowercase)
                .as_deref(),
            Some("if_demo")
        );
        assert_eq!(
            target
                .member_name()
                .and_then(|name| name.name(src))
                .as_deref(),
            Some("run")
        );
    }

    #[test]
    fn data_like_decl_helpers_expose_storage_signature_and_clause_names() {
        let src = "STATICS sv_count TYPE i.\nCLASS lcl DEFINITION.\n  PUBLIC SECTION.\n    CLASS-DATA gv_value TYPE i.\n    CONSTANTS:\n      BEGIN OF gc_struct,\n        p0 TYPE i VALUE 1,\n      END OF gc_struct.\n  PRIVATE SECTION.\n    DATA mv_value TYPE i.\nENDCLASS.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);

        let statics = DataLikeDecl::cast(SyntaxNodeRef::new(
            &parsed.file,
            parsed
                .file
                .find_first_kind(parsed.file.root(), SyntaxKind::StaticsDecl)
                .expect("statics decl"),
        ))
        .expect("statics decl");
        assert_eq!(statics.storage_kind(src), Some(DataLikeStorageKind::Static));
        assert_eq!(statics.signature_text(src), "STATICS sv_count TYPE i");
        assert_eq!(
            statics
                .clauses()
                .next()
                .and_then(|clause| clause.declared_name(src))
                .map(|(name, _)| name)
                .as_deref(),
            Some("sv_count")
        );

        let mut class_data_and_instance = parsed
            .file
            .children(parsed.file.root())
            .filter(|&child| parsed.file.kind(child) == SyntaxKind::ClassDecl)
            .flat_map(|class_decl| parsed.file.children(class_decl))
            .filter(|&child| parsed.file.kind(child) == SyntaxKind::DataDecl)
            .map(|node| {
                DataLikeDecl::cast(SyntaxNodeRef::new(&parsed.file, node)).expect("data decl")
            })
            .collect::<Vec<_>>();
        let class_data = class_data_and_instance.remove(0);
        let instance_data = class_data_and_instance.remove(0);
        assert_eq!(
            class_data.storage_kind(src),
            Some(DataLikeStorageKind::Static)
        );
        assert_eq!(class_data.signature_text(src), "CLASS-DATA gv_value TYPE i");
        assert_eq!(
            class_data
                .clauses()
                .next()
                .and_then(|clause| clause.declared_name(src))
                .map(|(name, _)| name)
                .as_deref(),
            Some("gv_value")
        );
        assert_eq!(
            instance_data.storage_kind(src),
            Some(DataLikeStorageKind::Instance)
        );
        assert_eq!(instance_data.signature_text(src), "DATA mv_value TYPE i");

        let constants = DataLikeDecl::cast(SyntaxNodeRef::new(
            &parsed.file,
            parsed
                .file
                .find_first_kind(parsed.file.root(), SyntaxKind::ConstantsDecl)
                .expect("constants decl"),
        ))
        .expect("constants decl");
        assert_eq!(
            constants.storage_kind(src),
            Some(DataLikeStorageKind::Constant)
        );
        assert!(
            constants
                .signature_text(src)
                .starts_with("CONSTANTS BEGIN OF gc_struct")
        );
        assert_eq!(
            constants
                .clauses()
                .next()
                .and_then(|clause| clause.declared_name(src))
                .map(|(name, _)| name)
                .as_deref(),
            Some("gc_struct")
        );
    }

    #[test]
    fn parses_interface_load_stmt_inside_interface_body_without_nested_endinterface() {
        let parsed = crate::parse(
            "INTERFACE if_outer.\n  INTERFACE if_inner LOAD.\n  METHODS run.\nENDINTERFACE.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::InterfaceDecl),
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
    fn parses_select_projection_case_expression() {
        let parsed = crate::parse(
            "SELECT col_a,\n\
                    CASE\n\
                      WHEN col_b = 'X' THEN '1'\n\
                      WHEN col_b = 'Y' THEN '2'\n\
                      ELSE '0'\n\
                    END AS priority\n\
               FROM ztab\n\
               INTO TABLE @DATA(lt_rows).",
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
    fn parses_multiline_select_projection_case_expression_with_following_clauses() {
        let parsed = crate::parse(
            "SELECT rep_evtid,\n\
                    evtid,\n\
                    rule_type,\n\
                    status_rep_evt,\n\
                    msguid_out,\n\
                    ext_ref_id,\n\
                    CASE\n\
                      WHEN ext_ref_id = 'PRIORITY1' THEN 'X'\n\
                      WHEN ext_ref_id = 'EXCLUDED' THEN 'X'\n\
                      WHEN ext_ref_id = 'PRIORITY2' THEN 'Y'\n\
                      ELSE ' '\n\
                    END AS priority,\n\
                    creation_time\n\
               FROM /sttp/rep_evt\n\
               INTO TABLE @DATA(lt_rep_evt)\n\
               UP TO @lv_bj2_max ROWS\n\
               WHERE status_rep_evt = @lc_sts_rep\n\
               AND rule_type IN @lr_sr_rule.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SelectStmt), 1);
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SqlProjectionItem),
            8
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectIntoClause),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectUpToClause),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectWhereClause),
            1
        );
    }

    #[test]
    fn parses_select_package_size_as_endselect_loop_clause() {
        let src = "SELECT * FROM demo INTO TABLE lt_rows PACKAGE SIZE @lv_pack.\n  WRITE lines( lt_rows ).\nENDSELECT.\nWRITE 'done'.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SelectStmt), 1);
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::SelectPackageSizeClause),
            1
        );
        let select = parsed
            .file
            .find_first_kind(root, SyntaxKind::SelectStmt)
            .and_then(|node| SelectStmt::cast(SyntaxNodeRef::new(&parsed.file, node)))
            .expect("SELECT statement");
        assert_eq!(
            select
                .non_query_children()
                .filter(|child| child.kind() == SyntaxKind::WriteStmt)
                .count(),
            1
        );
        assert!(
            select
                .syntax()
                .text(src)
                .is_some_and(|text| text.contains("ENDSELECT"))
        );
    }

    #[test]
    fn parses_select_for_update_offset_and_abap_options() {
        let parsed = crate::parse(
            "SELECT SINGLE FOR UPDATE * FROM demo INTO @DATA(ls_row) OFFSET @lv_skip BYPASSING BUFFER CONNECTION @lv_conn.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SelectStmt), 1);
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::SelectForUpdateClause),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectOffsetClause),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::SelectAbapOptionsClause),
            2
        );
    }

    #[test]
    fn parses_grouped_aggregate_select_as_endselect_loop() {
        let src = "SELECT carrid, COUNT( * ) FROM sflight GROUP BY carrid INTO ( lv_carrid, lv_count ).\n  WRITE lv_carrid.\nENDSELECT.\nWRITE 'done'.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        let select = parsed
            .file
            .find_first_kind(root, SyntaxKind::SelectStmt)
            .and_then(|node| SelectStmt::cast(SyntaxNodeRef::new(&parsed.file, node)))
            .expect("SELECT statement");
        assert!(
            select
                .syntax()
                .text(src)
                .is_some_and(|text| text.contains("ENDSELECT"))
        );
        assert_eq!(
            select
                .non_query_children()
                .filter(|child| child.kind() == SyntaxKind::WriteStmt)
                .count(),
            1
        );
    }

    #[test]
    fn parses_select_set_operator_tail_without_statement_split() {
        let parsed = crate::parse(
            "SELECT carrid FROM scarr\nUNION ALL SELECT carrid FROM spfli\nINTO TABLE @DATA(lt_ids).",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SelectStmt), 1);
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::SelectSetOperatorClause),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectIntoClause),
            1
        );
    }

    #[test]
    fn parses_leave_list_processing_stmt() {
        let parsed = crate::parse("LEAVE LIST-PROCESSING.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::LeaveStmt),
            1
        );
    }

    #[test]
    fn parses_concatenate_stmt() {
        let parsed =
            crate::parse("CONCATENATE 'Document' mv_odlv INTO lv_delivery_msg SEPARATED BY ': '.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::ConcatenateStmt),
            1
        );
    }

    #[test]
    fn parses_concatenate_stmt_with_inline_data_target_and_substrings() {
        let parsed = crate::parse(
            "DATA(lv_evttime) = '20260401000000'.\n\
CONCATENATE lv_evttime+6(4) '-'\n\
            lv_evttime+3(2) '-'\n\
            lv_evttime+0(2) 'T'\n\
            lv_evttime+11(8) '.000Z' INTO DATA(lv_timestp).",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::ConcatenateStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::DataInlineDecl), 2);
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::ConcatenateTargetOperand),
            1
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_grouped_concatenate_stmt() {
        let parsed = crate::parse(
            "CONCATENATE: TEXT-010 ls_zatt_transloading-vhcnum INTO lv_question SEPARATED BY space,\n\
                         lv_question TEXT-041                 INTO lv_question SEPARATED BY space.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::ConcatenateStmt), 1);
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::ConcatenateSourceOperand),
            4
        );
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::ConcatenateTargetOperand),
            2
        );
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::ConcatenateSeparatorOperand),
            2
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_split_stmt_with_multiline_into_targets_and_character_mode() {
        let parsed = crate::parse(
            "SPLIT iv_sgtin\nAT ':'\nINTO lv_part_1 lv_part_2 lv_part_3 lv_part_4 lv_part_5 lv_part_6\nIN CHARACTER MODE.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SplitStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::ExprIdent), 7);
    }

    #[test]
    fn parses_split_stmt_with_multiline_inline_data_targets() {
        let parsed = crate::parse(
            "SPLIT lv_gln_enc AT ':' INTO DATA(lv_urn) DATA(epc) DATA(id)\n  DATA(lv_sgln) DATA(lv_gln).",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SplitStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::DataInlineDecl), 5);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_split_stmt_into_table_inline_data_target() {
        let parsed = crate::parse("SPLIT ls_trn-trncode AT ':' INTO TABLE DATA(lt_split).");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SplitStmt), 1);
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SplitSourceOperand),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::SplitSeparatorOperand),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SplitTargetOperand),
            1
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::DataInlineDecl), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_chained_split_stmt_into_tables() {
        let parsed = crate::parse(
            "SPLIT: gs_user_creation-user_role AT ',' INTO TABLE lt_roles,\n\
                    gs_user_creation-gln       AT ',' INTO TABLE lt_glns.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SplitStmt), 1);
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SplitSourceOperand),
            2
        );
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::SplitSeparatorOperand),
            2
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SplitTargetOperand),
            2
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_condense_stmt() {
        let parsed = crate::parse("CONDENSE lv_datestring.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::CondenseStmt),
            1
        );
    }

    #[test]
    fn parses_condense_stmt_no_gaps() {
        let parsed = crate::parse("CONDENSE lv_gs1_element_delimiter NO-GAPS.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::CondenseStmt),
            1
        );
    }

    #[test]
    fn parses_classic_refresh_collect_free_and_unassign_statements() {
        let parsed = crate::parse(
            "REFRESH: lt_data_ext, lt_encode_decode.\n\
COLLECT ls_archstats_del_line INTO gt_archstats_del.\n\
FREE lt_data_ext.\n\
FREE MEMORY ID MEMORY_ID.\n\
UNASSIGN <fs_choice>.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::RefreshStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::RefreshOperand), 2);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::CollectStmt), 1);
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::CollectSourceOperand),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::CollectTargetOperand),
            1
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::FreeStmt), 2);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::FreeOperand), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::MemoryIdOperand), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnassignStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnassignOperand), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_multiline_chained_refresh_with_statement_keyword_operand() {
        let parsed = crate::parse(
            "REFRESH: ITEMS[],\n\
           ALL_ITEMS[],\n\
*          FILTER_DESCRIPTION[],\n\
           SELECTION[].\n\
CALL METHOD transport_from_model( my_model ).",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::RefreshStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::RefreshOperand), 3);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::CallMethodStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_import_and_export_memory_id_statements() {
        let parsed = crate::parse(
            "IMPORT lt_mem_return TO lt_return FROM MEMORY ID 'ZATTP_3PL_OER'.\n\
IMPORT <fs_choice> TO lt_choice1 FROM MEMORY ID 'LV_CHOICE'.\n\
EXPORT lv_bizstep FROM cs_event_data-bizstep TO MEMORY ID 'LV_BIZSTEP'.\n\
EXPORT ls_aup_parent_evt\n\
  FROM ls_aup_parent_evt\n\
  TO MEMORY ID  'MMID_AUP' .",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::ImportMemoryStmt),
            2
        );
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::ImportMemorySourceOperand),
            2
        );
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::ImportMemoryTargetOperand),
            2
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::ExportMemoryStmt),
            2
        );
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::ExportMemoryNameOperand),
            2
        );
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::ExportMemorySourceOperand),
            2
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::MemoryIdOperand), 4);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_import_and_export_data_buffer_statements_with_named_payloads() {
        let parsed = crate::parse(
            "EXPORT\n\
               INSERT_DATA_TAB = lt_insert_data\n\
               UPDATE_DATA_TAB = lt_update_data\n\
               DELETE_TAB      = lt_delete_data\n\
                 to data buffer xcontainer.\n\
             IMPORT\n\
               INSERT_DATA_TAB = lt_insert_data\n\
               from data buffer xcontainer.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::ExportMemoryStmt),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::ImportMemoryStmt),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::DataBufferOperand),
            2
        );
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::ExportMemorySourceOperand),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::ImportMemoryTargetOperand),
            1
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::AssignStmt), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn condense_stmt_no_gaps_is_not_picked_up_inside_parens() {
        let parsed = crate::parse("CONDENSE func( NO-GAPS ).");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::CondenseStmt),
            1
        );
    }

    #[test]
    fn write_and_concatenate_build_expression_children() {
        let parsed =
            crate::parse("WRITE / lo_prog->to_string( ). CONCATENATE lv_a lv_b INTO lv_text.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::ConcatenateSourceOperand),
            2
        );
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::ConcatenateTargetOperand),
            1
        );
        assert!(parsed.file.count_kind(root, SyntaxKind::CallExpr) >= 1);
        assert!(parsed.file.count_kind(root, SyntaxKind::SelectorExpr) >= 1);
        assert!(parsed.file.count_kind(root, SyntaxKind::ExprIdent) >= 3);
    }

    #[test]
    fn write_position_literal_keeps_following_selector_as_operand() {
        let src = "WRITE: /5 ls_outers-parent_epc.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SelectorExpr), 1);

        let write = parsed
            .file
            .find_first_kind(root, SyntaxKind::WriteStmt)
            .and_then(|node| WriteStmt::cast(SyntaxNodeRef::new(&parsed.file, node)))
            .expect("WRITE statement");
        let operands = write.operands().collect::<Vec<_>>();
        assert_eq!(operands.len(), 1);
        assert_eq!(operands[0].text(src), Some("ls_outers-parent_epc"));
    }

    #[test]
    fn parses_flat_select_into_tuple_without_endselect() {
        let parsed = crate::parse(
            "SELECT MAX( bup_role_variant ) COUNT( * ) INTO ( lv_max, lv_count ) FROM demo. IF lv_count > 0. ENDIF.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SelectStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::IfStmt), 1);
        let into_clause = parsed
            .file
            .find_first_kind(root, SyntaxKind::SelectIntoClause)
            .and_then(|node| SelectIntoClause::cast(SyntaxNodeRef::new(&parsed.file, node)))
            .expect("SELECT INTO clause");
        assert_eq!(into_clause.target_children().count(), 2);
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
    fn parses_flat_select_into_table_inline_data_target() {
        let parsed = crate::parse(
            "SELECT rfcdest FROM rfcdes INTO TABLE @DATA(lt_rfcdes) WHERE mandt = sy-mandt.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SelectStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::DataInlineDecl), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SelectQuery), 1);
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::SelectProjectionList),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectFromClause),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectIntoClause),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectWhereClause),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SqlProjectionItem),
            1
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SqlColumnRef), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SqlDataSource), 1);
    }

    #[test]
    fn parses_legacy_select_join_into_corresponding_fields_endselect() {
        let parsed = crate::parse(
            "SELECT * FROM /sttp/bup AS a JOIN /sttp/bupmap AS b ON b~bupid = a~bupid INTO CORRESPONDING FIELDS OF ls_buffer_role WHERE b~bupid = iv_bupid. ENDSELECT.",
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
    fn parses_legacy_call_method_with_inline_importing_targets_and_trailing_comments() {
        let parsed = crate::parse(
            "CALL METHOD lo_obj->send_notification_acc\n  EXPORTING\n    it_acc_obj = lt_obj_comm\n  IMPORTING\n    ev_rep_status = DATA(lv_rep_status) \" Reporting Event Status\n    ev_http_code = DATA(lv_http_code). \" Character Field Length = 10",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::CallMethodStmt)
            .expect("call method stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallNamedArg), 3);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::DataInlineDecl), 2);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_legacy_call_method_with_parenthesized_named_sections() {
        let parsed = crate::parse(
            "CALL METHOD populate_codes(\n  EXPORTING\n    iv_rule_type = iv_rule_type\n    is_req_data  = <fs_req_data>\n  IMPORTING\n    et_kodovi    = DATA(lt_kodovi)\n    et_kod_all   = DATA(lt_kodovi_all) ).",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::CallMethodStmt)
            .expect("call method stmt");
        assert_eq!(
            parsed.file.count_kind(stmt, SyntaxKind::CallMethodTarget),
            1
        );
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallExpr), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallArgSection), 2);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallNamedArg), 4);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::DataInlineDecl), 2);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_legacy_call_method_with_dynamic_instance_target() {
        let parsed = crate::parse("CALL METHOD lo_obj->(l_method) RECEIVING result = lv_result.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::CallMethodStmt)
            .expect("call method stmt");
        assert_eq!(
            parsed.file.count_kind(stmt, SyntaxKind::CallMethodTarget),
            1
        );
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::SelectorExpr), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::ParenExpr), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallArgSection), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallNamedArg), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::Error), 0);
    }

    #[test]
    fn legacy_call_method_builds_structured_argument_list() {
        let parsed = crate::parse(
            "CALL METHOD zcl_demo=>get_hash EXPORTING iv_text = mv_text RECEIVING rv_hash = DATA(lv_hash).",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::CallMethodStmt)
            .expect("call method stmt");
        assert_eq!(
            parsed.file.count_kind(stmt, SyntaxKind::CallMethodTarget),
            1
        );
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::SelectorExpr), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallArgList), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallArgSection), 2);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallNamedArg), 2);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::DataInlineDecl), 1);
    }

    #[test]
    fn parses_legacy_call_method_with_exceptions_section() {
        let parsed = crate::parse(
            "CALL METHOD cl_gui_frontend_services=>file_open_dialog\n  CHANGING\n    file_table = lt_files\n    rc = lv_rc\n    user_action = lv_action\n  EXCEPTIONS\n    OTHERS = 1.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::CallMethodStmt)
            .expect("call method stmt");
        assert_eq!(
            parsed.file.count_kind(stmt, SyntaxKind::CallMethodTarget),
            1
        );
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallArgSection), 2);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallNamedArg), 4);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::Error), 0);
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
    fn parses_create_data_with_dynamic_type_as_one_statement() {
        let parsed = crate::parse("CREATE DATA lr_sap_data TYPE (ls_finf-ddicstructure).");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::CreateDataStmt)
            .expect("create data stmt");
        assert!(parsed.file.count_kind(stmt, SyntaxKind::ExprIdent) >= 2);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::SelectorExpr), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_create_data_like_as_one_statement() {
        let parsed = crate::parse("CREATE DATA mo_outbound LIKE iv_data.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::CreateDataStmt)
            .expect("create data stmt");
        assert!(parsed.file.count_kind(stmt, SyntaxKind::ExprIdent) >= 2);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_get_reference_of_into_as_one_statement() {
        let parsed =
            crate::parse("GET REFERENCE OF es_request_aif_struct INTO ls_xmlparse-xi_data.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::GetReferenceStmt)
            .expect("get reference stmt");
        assert!(parsed.file.count_kind(stmt, SyntaxKind::ExprIdent) >= 2);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::SelectorExpr), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_get_reference_of_into_object_ref_target() {
        let parsed = crate::parse("GET REFERENCE OF ls_xmlparse INTO lo_xmlparse.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::GetReferenceStmt)
            .expect("get reference stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::ExprIdent), 2);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::Error), 0);
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
    fn parses_read_table_with_table_key_components_on_continuation_lines() {
        let parsed = crate::parse(
            "READ TABLE lt_rows\n  WITH TABLE KEY\n    FUNCTION = lv_function\n    id = lv_id\n  TRANSPORTING NO FIELDS.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::ReadTableStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_read_table_into_inline_data_before_with_key() {
        let parsed = crate::parse(
            "READ TABLE lt_t_param INTO DATA(ls_bj2_max) WITH KEY param_name = lc_rs_bj2_max.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::ReadTableStmt)
            .expect("read table stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::DataInlineDecl), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::TemplateExpr), 2);
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
    fn parses_multiline_read_table_assigning_inline_field_symbol() {
        let parsed = crate::parse(
            "READ TABLE lt_child_warning ASSIGNING\n  FIELD-SYMBOL(<fs_child_success>)\n  WITH KEY rep_evtid = ls_evt-rep_evtid.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::ReadTableStmt),
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
    fn parses_chained_read_table_stmt() {
        let parsed = crate::parse(
            "READ TABLE: lt_aux_dm_trn INTO ls_aux_dm_trn WITH KEY docnum = ls_zatt_ship_pending-docnum,\n  lt_aux_dm_trn_evt INTO ls_aux_dm_trn_evt WITH KEY trnid = ls_aux_dm_trn-trnid.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::ReadTableStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::TemplateExpr), 6);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_authority_check_with_for_user_and_dummy() {
        let parsed = crate::parse(
            "AUTHORITY-CHECK OBJECT lv_auth FOR USER lv_user\n  ID lv_field FIELD lv_value\n  ID lv_actvt DUMMY.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::AuthorityCheckStmt)
            .expect("authority-check stmt");
        assert_eq!(
            parsed
                .file
                .count_kind(stmt, SyntaxKind::AuthorityCheckObjectOperand),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(stmt, SyntaxKind::AuthorityCheckUserOperand),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(stmt, SyntaxKind::AuthorityCheckIdClause),
            2
        );
        assert_eq!(
            parsed
                .file
                .count_kind(stmt, SyntaxKind::AuthorityCheckIdOperand),
            2
        );
        assert_eq!(
            parsed
                .file
                .count_kind(stmt, SyntaxKind::AuthorityCheckFieldOperand),
            1
        );
    }

    #[test]
    fn parses_authority_check_literal_operands_as_ast_children() {
        let parsed = crate::parse(
            "AUTHORITY-CHECK OBJECT 'S_CARRID' ID 'CARRID' FIELD carr ID 'ACTVT' FIELD '03'.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::AuthorityCheckStmt)
            .expect("authority-check stmt");
        assert_eq!(
            parsed
                .file
                .count_kind(stmt, SyntaxKind::AuthorityCheckIdClause),
            2
        );
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::TemplateExpr), 5);
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
    fn parses_append_lines_of_operands_as_ast_children() {
        let parsed = crate::parse("APPEND LINES OF lt_src TO lt_dst.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::AppendStmt)
            .expect("append stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::TemplateExpr), 2);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::ExprIdent), 2);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_append_initial_line_assigning_operands_as_ast_children() {
        let parsed =
            crate::parse("APPEND INITIAL LINE TO lt_bup_reg_key ASSIGNING <ls_bup_reg_key>.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::AppendStmt)
            .expect("append stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::TemplateExpr), 2);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::ExprIdent), 2);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_insert_into_table_stmt() {
        let parsed = crate::parse("INSERT is_buffer INTO TABLE st_buffer_role.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::InsertTableStmt),
            1
        );
    }

    #[test]
    fn parses_insert_into_table_operands_as_ast_children() {
        let parsed = crate::parse("INSERT is_buffer INTO TABLE st_buffer_role.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::InsertTableStmt)
            .expect("insert into table stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::TemplateExpr), 2);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::ExprIdent), 2);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_insert_lines_of_into_table() {
        let parsed = crate::parse("INSERT LINES OF lt_src INTO TABLE lt_dst.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::InsertTableStmt),
            1
        );
    }

    #[test]
    fn insert_dbtab_from_wa_is_not_insert_table_stmt() {
        let parsed = crate::parse("INSERT ztab FROM wa.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::InsertTableStmt),
            0
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::InsertDbTableStmt),
            1
        );
    }

    #[test]
    fn parses_insert_textpool_operands_as_ast_children() {
        let parsed = crate::parse("INSERT TEXTPOOL program FROM text2 LANGUAGE langu2.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::InsertTextpoolStmt)
            .expect("insert textpool stmt");
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::InsertDbTableStmt),
            0
        );
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::TemplateExpr), 3);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::ExprIdent), 3);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_insert_textpool_without_language() {
        let parsed = crate::parse("INSERT TEXTPOOL lv_progname FROM lt_textpool.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::InsertTextpoolStmt)
            .expect("insert textpool stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::TemplateExpr), 2);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::ExprIdent), 2);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_insert_dbtab_from_table_operands_as_ast_children() {
        let parsed = crate::parse(
            "INSERT zattp_sequen_bf FROM TABLE lt_sequen_buff ACCEPTING DUPLICATE KEYS.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::InsertDbTableStmt)
            .expect("insert db table stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::SqlDataSource), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::TemplateExpr), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::ExprIdent), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_insert_into_itab_index_multiline() {
        let parsed = crate::parse("INSERT lv_parent_bupid\n  INTO   lt_bupid\n  INDEX  1.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::InsertTableStmt),
            1
        );
    }

    #[test]
    fn insert_into_dbtab_values_is_not_insert_table_stmt() {
        let parsed = crate::parse("INSERT INTO customers VALUES wa.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::InsertTableStmt),
            0
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::InsertDbTableStmt),
            1
        );
    }

    #[test]
    fn parses_insert_into_dbtab_values_constructor_operands_as_ast_children() {
        let parsed = crate::parse(
            "INSERT INTO zattp_rs_ruleacc\n  VALUES @( VALUE #( parent_rule_rep = ls_rep_evt-rep_evtid\n                     child_rule_rep = <fs_repevtid>-rep_evtid ) ).",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::InsertDbTableStmt)
            .expect("insert db table stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::SqlDataSource), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::SqlHostExpr), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_insert_into_dynamic_dbtab_values_stmt() {
        let parsed = crate::parse("INSERT INTO (lv_master) VALUES im_pmast.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::InsertTableStmt),
            0
        );
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::InsertDbTableStmt)
            .expect("insert db table stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::SqlDataSource), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::ExprIdent), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_move_corresponding_operands_as_ast_children() {
        let parsed = crate::parse("MOVE-CORRESPONDING ls_general TO ls_ord_head.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::MoveCorrespondingStmt)
            .expect("move-corresponding stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::TemplateExpr), 2);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::ExprIdent), 2);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_move_to_operands_as_ast_children() {
        let parsed = crate::parse("MOVE it_gs1_check_table TO lt_gs1_gcp.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::MoveStmt)
            .expect("move stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::TemplateExpr), 2);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::ExprIdent), 2);
    }

    #[test]
    fn parses_multiline_chained_move_with_function_named_source() {
        let parsed = crate::parse(
            "FORM update_status.\n\
  MOVE: STATUS-DATA TO <STATUS>-ST_DATA,\n\
        STATUS-MODE TO <STATUS>-ST_MODE,\n\
*       L TO <STATUS>-CUR_LINE,\n\
        FUNCTION    TO <STATUS>-FCODE.\n\
ENDFORM.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::FormDecl), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::FunctionDecl), 0);
        let stmt = parsed
            .file
            .find_first_kind(root, SyntaxKind::MoveStmt)
            .expect("move stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::TemplateExpr), 6);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_sort_by_as_sort_stmt() {
        let parsed = crate::parse("SORT lt_gs1_gcp BY gs1_gcp.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::SortStmt)
            .expect("sort stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::TemplateExpr), 2);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::ExprIdent), 2);
    }

    #[test]
    fn parses_sort_by_multiple_fields_as_ast_children() {
        let parsed =
            crate::parse("SORT lt_gs1_gcp BY gs1_gcp ASCENDING matnr DESCENDING lgnum AS TEXT.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::SortStmt)
            .expect("sort stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::TemplateExpr), 4);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::ExprIdent), 4);
    }

    #[test]
    fn parses_modify_operands_as_ast_children() {
        let parsed = crate::parse("MODIFY zatt_trans_cust FROM ls_trans.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::ModifyStmt)
            .expect("modify stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::TemplateExpr), 2);
    }

    #[test]
    fn parses_modify_table_with_transporting_where() {
        let parsed = crate::parse(
            "MODIFY TABLE lt_items FROM ls_item TRANSPORTING qty WHERE id = ls_item-id.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::ModifyStmt),
            1
        );
    }

    #[test]
    fn parses_modify_screen_variants() {
        for src in ["MODIFY SCREEN.", "MODIFY SCREEN FROM ls_screen."] {
            let parsed = crate::parse(src);
            assert!(parsed.errors.is_empty(), "{src}: {:?}", parsed.errors);
            assert_eq!(
                parsed
                    .file
                    .count_kind(parsed.file.root(), SyntaxKind::ModifyStmt),
                1,
                "{src}"
            );
        }
    }

    #[test]
    fn parses_multiline_update_set_statement_without_frontend_error() {
        let parsed = crate::parse(
            "UPDATE zattp_rs_represp\n  SET reprocessing_status = 'S'\n      retry_count = <fs_rs_represp>-retry_count\n  WHERE rep_evtid EQ <fs_rs_represp>-rep_evtid.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UpdateStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UpdateSetClause), 1);
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::UpdateSetAssignment),
            2
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::UpdateWhereClause),
            1
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
    }

    #[test]
    fn parses_update_where_comparisons_on_continuation_lines() {
        let parsed = crate::parse(
            "UPDATE ekes SET menge = ets-menge\n* ormng = ets-ormng\n  dabmg = ets-menge WHERE\n    ebeln = ets-ebeln AND\n    ebelp = ets-ebelp AND\n    etens = ets-etens.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UpdateStmt), 1);
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::UpdateWhereClause),
            1
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_update_from_operand_as_ast_child() {
        let parsed = crate::parse("UPDATE /aif/fhead FROM ls_fhead.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UpdateStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SqlDataSource), 1);
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::UpdateFromOperand),
            1
        );
    }

    #[test]
    fn parses_update_dynamic_where_as_sql_dynamic_where() {
        let parsed =
            crate::parse("UPDATE idxrcvpor SET msg_deleted = lc_msg_deleted WHERE (where_clause).");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::UpdateStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SqlDynamicWhere), 1);
    }

    #[test]
    fn parses_delete_table_where_and_index_operands_as_ast_children() {
        let parsed = crate::parse(
            "DELETE lt_trans_del WHERE status_trn <> /sttp/cl_dm_constants=>gcs_stat_trn-deleted.\nDELETE lt_pay_header INDEX lv_index_hdr.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::DeleteStmt),
            2
        );
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::DeleteStmt)
            .expect("delete stmt");
        assert!(parsed.file.count_kind(stmt, SyntaxKind::BinaryExpr) >= 1);
        assert!(parsed.file.count_kind(stmt, SyntaxKind::SelectorExpr) >= 1);
    }

    #[test]
    fn parses_delete_adjacent_duplicates_from_clause() {
        let parsed = crate::parse("DELETE ADJACENT DUPLICATES FROM lt_gcp COMPARING gcp.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::DeleteStmt)
            .expect("delete stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::TemplateExpr), 2);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::ExprIdent), 2);
    }

    #[test]
    fn parses_delete_table_from_work_area_as_internal_table_delete() {
        let parsed = crate::parse("DELETE TABLE ct_objids FROM is_obj_ids.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::DeleteStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::DeleteDbTableStmt),
            0
        );
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::DeleteStmt)
            .expect("delete stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::TemplateExpr), 2);
    }

    #[test]
    fn parses_delete_from_table_clause_with_namespaced_dbtab() {
        let parsed = crate::parse("DELETE /sttp/bup_adr FROM TABLE lt_bupa_adr.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::DeleteDbTableStmt)
            .expect("delete stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::TemplateExpr), 2);
    }

    #[test]
    fn parses_assign_to_inline_field_symbol() {
        let parsed = crate::parse("ASSIGN mo_outbound->* TO FIELD-SYMBOL(<ls_outbound>).");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::AssignKeywordStmt)
            .expect("assign keyword stmt");
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::AssignKeywordStmt),
            1
        );
        assert_eq!(
            parsed.file.count_kind(stmt, SyntaxKind::AssignSourceExpr),
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
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::AssignKeywordStmt)
            .expect("assign keyword stmt");
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::AssignKeywordStmt),
            1
        );
        assert_eq!(
            parsed.file.count_kind(stmt, SyntaxKind::AssignSourceExpr),
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
    fn parses_assign_component_multiline_to_inline_field_symbol() {
        let parsed = crate::parse(
            "ASSIGN COMPONENT 'ADD_FIELDS' OF STRUCTURE <ls_outbound> TO\n  FIELD-SYMBOL(<ls_add_fields>) ##no_text.",
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
    fn parses_assign_to_inline_field_symbol_with_casting_type() {
        let parsed =
            crate::parse("ASSIGN lo_data->* TO FIELD-SYMBOL(<ls_data>) CASTING TYPE ty_row.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::AssignKeywordStmt)
            .expect("assign keyword stmt");
        assert_eq!(
            parsed.file.count_kind(stmt, SyntaxKind::AssignSourceExpr),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(stmt, SyntaxKind::FieldSymbolInlineDecl),
            1
        );
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::TypeRefSimple), 1);
    }

    #[test]
    fn parses_assign_with_dynamic_casting_type_expression() {
        let parsed = crate::parse("ASSIGN lo_data->* TO <ls_data> CASTING TYPE (lv_gentab).");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::AssignKeywordStmt)
            .expect("assign keyword stmt");
        assert_eq!(
            parsed.file.count_kind(stmt, SyntaxKind::AssignSourceExpr),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(stmt, SyntaxKind::FieldSymbolInlineDecl),
            0
        );
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::TypeRefSimple), 0);
        assert!(
            parsed
                .file
                .find_first_kind(stmt, SyntaxKind::ParenExpr)
                .is_some(),
            "expected dynamic CASTING TYPE operand to remain as an expression"
        );
    }

    #[test]
    fn rejects_whitespace_inside_inline_field_symbol_parentheses() {
        let parsed = crate::parse("ASSIGN mo_outbound->* TO FIELD-SYMBOL( <ls_outbound>).");
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::FieldSymbolInlineDecl),
            0
        );
        assert!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::Error)
                >= 1
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
    fn parses_select_join_dynamic_where_and_for_all_entries_structurally() {
        let parsed = crate::parse(
            "SELECT DISTINCT a~bupid, b~*\n  FROM /sttp/bup AS a\n  JOIN /sttp/bupmap AS b ON b~bupid = a~bupid\n  FOR ALL ENTRIES IN lt_keys\n  INTO TABLE @DATA(lt_rows)\n  WHERE (lt_cond)\n  ORDER BY PRIMARY KEY.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SelectQuery), 1);
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::SelectDistinctClause),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::SelectProjectionList),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectFromClause),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectJoinClause),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::SelectForAllEntriesClause),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectIntoClause),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectWhereClause),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::SelectOrderByClause),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SqlQualifiedStar),
            1
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SqlDynamicWhere), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SqlAlias), 2);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SqlDataSource), 2);
    }

    #[test]
    fn parses_structured_sql_projection_and_predicate_operands() {
        let parsed = crate::parse(
            "SELECT MAX( a~bupid ) AS max_bupid\n  FROM /sttp/bup AS a\n  WHERE a~bupid = @mv_bupid\n  AND status = iv_status\n  INTO @DATA(lv_bupid).",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SqlAggregateCall),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::SqlQualifiedColumnRef),
            2
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SqlAliasClause), 2);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SqlHostExpr), 1);
        assert!(
            parsed
                .file
                .count_kind(root, SyntaxKind::SqlPredicateOperand)
                >= 4
        );
    }

    #[test]
    fn parses_select_single_from_fields_structurally() {
        let parsed = crate::parse(
            "SELECT SINGLE\n  FROM /sttp/rep_evt\n  FIELDS rep_evtid,\n         rule_type,\n         msguid_out\n  WHERE evtid = @mv_evtid\n  AND rule_type IN (\n    @zattp_cl_rs_rule_proc=>gcs_rule_type-shipping,\n    @zattp_cl_rs_rule_proc=>gcs_rule_type-transloading )\n  AND status_rep_evt = 1\n  AND recall_status = 3\n  INTO @DATA(ls_rep_evt).",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SelectStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SelectQuery), 1);
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::SelectProjectionList),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectFromClause),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectIntoClause),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectWhereClause),
            1
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SqlDataSource), 1);
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SqlProjectionItem),
            3
        );
    }

    #[test]
    fn parses_classic_select_single_into_inline_data_before_where() {
        let parsed = crate::parse(
            "SELECT SINGLE vhcnum\n  FROM zattp_tnc_portal\n  INTO @DATA(lv_vozilooznaka)\n  WHERE docnum = @mv_odlv\n  AND legisl_del = 'RS'.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SelectStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SelectQuery), 1);
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::SelectProjectionList),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectFromClause),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectIntoClause),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectWhereClause),
            1
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::DataInlineDecl), 1);
    }

    #[test]
    fn parses_classic_select_single_into_inline_data_with_commented_where_line() {
        let parsed = crate::parse(
            "SELECT SINGLE vhcnum\n  FROM zattp_tnc_portal\n  INTO @DATA(lv_vozilooznaka)\n* WHERE docnum = @lv_odlv\n  WHERE docnum = @mv_odlv\n  AND legisl_del = 'RS'.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SelectStmt), 1);
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectIntoClause),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectWhereClause),
            1
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::DataInlineDecl), 1);
    }

    #[test]
    fn parses_select_projection_comments_and_old_tail_order() {
        let parsed = crate::parse(
            "SELECT rep_evtid,                                                   \"Reporting Event id\n         evtid,\n*        rule_type,\n         status_response\n  APPENDING CORRESPONDING FIELDS OF TABLE et_rep_dep\n  FROM /sttp/rep_dep\n  FOR ALL ENTRIES IN lt_keys\n  WHERE rep_evtid = lt_keys-rep_evtid.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SelectStmt), 1);
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::SelectProjectionList),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SqlProjectionItem),
            3
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectIntoClause),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectFromClause),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::SelectForAllEntriesClause),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectWhereClause),
            1
        );
    }

    #[test]
    fn parses_open_cursor_with_hold_and_for_all_entries_query() {
        let parsed = crate::parse(
            "OPEN CURSOR WITH HOLD @lv_cursor\n  FOR\n  SELECT a~objid,\n         c~gtin\n  FROM /sttp/dm_obj AS a\n  JOIN /sttp/dm_obj_ids AS b ON a~objid = b~objid\n  LEFT JOIN /sttp/dm_obj_itm AS c ON a~objid = c~objid\n  FOR ALL ENTRIES IN @lt_event_rel\n  WHERE a~objid = @lt_event_rel-objid\n    AND ( b~storage = @/sttp/cl_constants=>gcs_storage-active_hot\n       OR b~storage = @/sttp/cl_constants=>gcs_storage-active_cold ).",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::OpenCursorStmt), 1);
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::CursorHandleOperand),
            1
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SelectQuery), 1);
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectFromClause),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectJoinClause),
            2
        );
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::SelectForAllEntriesClause),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectWhereClause),
            1
        );
        let stmt = parsed
            .file
            .find_first_kind(root, SyntaxKind::OpenCursorStmt)
            .and_then(|node| OpenCursorStmt::cast(SyntaxNodeRef::new(&parsed.file, node)))
            .expect("open cursor stmt");
        assert!(stmt.handle().is_some());
        assert!(stmt.query().is_some());
    }

    #[test]
    fn open_cursor_stmt_does_not_cascade_into_following_method() {
        let parsed = crate::parse(
            "CLASS lcl DEFINITION.\n  PUBLIC SECTION.\n    METHODS first.\n    METHODS second.\nENDCLASS.\nCLASS lcl IMPLEMENTATION.\n  METHOD first.\n    OPEN CURSOR WITH HOLD lv_cursor\n    FOR\n    SELECT *\n    FROM /sttp/dm_obj_ids\n    WHERE (lt_sql_cond).\n  ENDMETHOD.\n  METHOD second.\n  ENDMETHOD.\nENDCLASS.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::ClassDecl), 2);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::MethodDecl), 2);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::OpenCursorStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_fetch_next_cursor_into_table_package_size() {
        let parsed = crate::parse(
            "FETCH NEXT CURSOR @lv_cursor INTO TABLE @lt_lot_items PACKAGE SIZE @iv_size_lot_items.\nIF sy-subrc <> 0.\nENDIF.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::FetchCursorStmt), 1);
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::CursorHandleOperand),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectIntoClause),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::SelectPackageSizeClause),
            1
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::IfStmt), 1);
        let stmt = parsed
            .file
            .find_first_kind(root, SyntaxKind::FetchCursorStmt)
            .and_then(|node| FetchCursorStmt::cast(SyntaxNodeRef::new(&parsed.file, node)))
            .expect("fetch cursor stmt");
        assert!(stmt.handle().is_some());
    }

    #[test]
    fn parses_fetch_next_cursor_appending_corresponding_package_size() {
        let parsed = crate::parse(
            "FETCH NEXT CURSOR s_cursor\n  APPENDING CORRESPONDING FIELDS OF TABLE lt_mcex_cnf_s_maa\n  PACKAGE SIZE s_maximum_size.\nIF sy-subrc EQ 0.\nENDIF.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::FetchCursorStmt), 1);
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::CursorHandleOperand),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectIntoClause),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::SelectPackageSizeClause),
            1
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::IfStmt), 1);
        let stmt = parsed
            .file
            .find_first_kind(root, SyntaxKind::FetchCursorStmt)
            .and_then(|node| FetchCursorStmt::cast(SyntaxNodeRef::new(&parsed.file, node)))
            .expect("fetch cursor stmt");
        assert!(stmt.handle().is_some());
    }

    #[test]
    fn parses_close_cursor_stmt_with_host_handle() {
        let parsed = crate::parse("CLOSE CURSOR @lv_cursor.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::CloseCursorStmt), 1);
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::CursorHandleOperand),
            1
        );
        let stmt = parsed
            .file
            .find_first_kind(root, SyntaxKind::CloseCursorStmt)
            .and_then(|node| CloseCursorStmt::cast(SyntaxNodeRef::new(&parsed.file, node)))
            .expect("close cursor stmt");
        assert!(stmt.handle().is_some());
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
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::RaiseStmt)
            .expect("raise stmt");
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::RaiseStmt),
            1
        );
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::TypeRefSimple), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallArgList), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallArgSection), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallNamedArg), 2);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::AssignStmt),
            0
        );
    }

    #[test]
    fn parses_raise_exception_type_with_statement_keyword_named_arg() {
        let parsed = crate::parse(
            "RAISE EXCEPTION TYPE zcx_feedback\n  EXPORTING\n    textid = zcx_feedback=>http_error\n    method = 'CREATE_DEEP_ENTITY'.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::RaiseStmt)
            .expect("raise stmt");
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::RaiseStmt),
            1
        );
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallArgList), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallArgSection), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallNamedArg), 2);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::MethodDecl),
            0
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::AssignStmt),
            0
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::Error),
            0
        );
    }

    #[test]
    fn parses_raise_exception_type_period_form_with_namespaced_class() {
        let parsed = crate::parse("RAISE EXCEPTION TYPE /sttp/cx_rep_exception.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::RaiseStmt)
            .expect("raise stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::TypeRefSimple), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_message_stmt_multiline_id_form() {
        let parsed = crate::parse(
            "METHOD m.\n\
               MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno\n\
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4\n\
                 INTO cv_dummy_msg.\n\
             ENDMETHOD.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::MessageStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::MessageHeadClause),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::MessageWithClause),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::MessageIntoClause),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::MessageIdOperand),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::MessageTypeOperand),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::MessageNumberOperand),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::MessageOperand),
            4
        );
    }

    #[test]
    fn parses_message_stmt_dynamic_type() {
        let parsed = crate::parse("METHOD m.\n  MESSAGE lv_result TYPE 'E'.\nENDMETHOD.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::MessageStmt),
            1
        );
    }

    #[test]
    fn parses_message_stmt_compact_class_with_literal_and_display_like() {
        let parsed = crate::parse(
            "METHOD m.\n  MESSAGE s398(00) WITH 'Previous job is still processing' DISPLAY LIKE 'E'.\nENDMETHOD.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::MessageStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::MessageHeadClause),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::MessageWithClause),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::MessageDisplayLikeClause),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::MessageCodeOperand),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::MessageOperand),
            1
        );
    }

    #[test]
    fn parses_message_stmt_into_data_clause_with_inline_decl_child() {
        let parsed = crate::parse(
            "METHOD m.\n  MESSAGE w899(/sttp/msg) WITH sy-msgv1 INTO DATA(lv_message).\nENDMETHOD.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let into_clause = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::MessageIntoClause)
            .expect("message into clause");
        assert_eq!(
            parsed
                .file
                .count_kind(into_clause, SyntaxKind::DataInlineDecl),
            1
        );
        assert_eq!(parsed.file.count_kind(into_clause, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_message_stmt_with_text_pool_operand_node() {
        let parsed =
            crate::parse("METHOD m.\n  MESSAGE s398(00) WITH TEXT-007 lv_name.\nENDMETHOD.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::MessageTextPoolId),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::MessageOperand),
            1
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
    fn parses_get_time_stamp_field_stmt() {
        let parsed = crate::parse("GET TIME STAMP FIELD lv_ts.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::GetTimeStampStmt),
            1
        );
    }

    #[test]
    fn parses_get_time_stamp_field_inline_data_target() {
        let parsed = crate::parse("GET TIME STAMP FIELD DATA(lv_ts).");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::GetTimeStampStmt)
            .expect("get time stamp stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::DataInlineDecl), 1);
    }

    #[test]
    fn parses_get_bit_and_set_bit_multiline() {
        let src = "METHOD m.\n\
            GET BIT lv_bit_pos_source\n\
            OF      iv_tag\n\
            INTO    lv_bit_value.\n\
            SET BIT lv_bit_pos_target\n\
            OF      lv_x_100\n\
            TO      lv_bit_value.\n\
            ENDMETHOD.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::GetBitStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::SetBitStmt),
            1
        );
    }

    #[test]
    fn parses_get_bit_inline_data_into_target() {
        let parsed = crate::parse("GET BIT lv_pos OF lv_x INTO DATA(lv_b).");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::GetBitStmt)
            .expect("get bit stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::DataInlineDecl), 1);
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
    fn parses_system_function_call_with_id_field_pairs() {
        let src = "CALL 'ThWpInfo' ID 'OPCODE' FIELD opcode_wp_get_info \" #EC CI_CCALL\n  ID 'DIAWP' FIELD num_dia_wps\n  ID 'FREE_DIAWP' FIELD num_free_dia_wps\n  ID 'BTCWP' FIELD num_btc_wps\n  ID 'FREE_BTCWP' FIELD num_free_btc_wps\n  ID 'LOAD_INFO' FIELD load_info\n  ID 'SERVER_NAME' FIELD ls_msglist-name.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt_id = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::CallStmt)
            .expect("system call stmt");
        let stmt = CallStmt::cast(SyntaxNodeRef::new(&parsed.file, stmt_id)).expect("call stmt");
        assert_eq!(stmt.call_kind(src), Some(CallStmtKind::SystemFunction));
        assert!(stmt.system_function_callee().is_some());
        assert_eq!(parsed.file.count_kind(stmt_id, SyntaxKind::CallArgList), 1);
        assert_eq!(
            parsed.file.count_kind(stmt_id, SyntaxKind::CallArgSection),
            14
        );
        assert_eq!(
            parsed
                .file
                .count_kind(stmt_id, SyntaxKind::CallPositionalArg),
            14
        );
        assert_eq!(parsed.file.count_kind(stmt_id, SyntaxKind::SelectorExpr), 1);
        assert_eq!(parsed.file.count_kind(stmt_id, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_system_function_call_with_dynamic_operands_and_table_field() {
        let src = "CALL lv_cfunc ID lv_parameter FIELD lt_rows[].";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt_id = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::CallStmt)
            .expect("system call stmt");
        let stmt = CallStmt::cast(SyntaxNodeRef::new(&parsed.file, stmt_id)).expect("call stmt");
        assert_eq!(stmt.call_kind(src), Some(CallStmtKind::SystemFunction));
        assert_eq!(
            parsed.file.count_kind(stmt_id, SyntaxKind::CallArgSection),
            2
        );
        assert_eq!(
            parsed
                .file
                .count_kind(stmt_id, SyntaxKind::CallPositionalArg),
            2
        );
        assert_eq!(parsed.file.count_kind(stmt_id, SyntaxKind::TableExpr), 1);
        assert_eq!(parsed.file.count_kind(stmt_id, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_system_function_call_keyword_named_operands_as_values() {
        for src in ["CALL id.", "CALL 'C_FUNC' ID field FIELD id."] {
            let parsed = crate::parse(src);
            assert!(parsed.errors.is_empty(), "{src}: {:?}", parsed.errors);
            let stmt_id = parsed
                .file
                .find_first_kind(parsed.file.root(), SyntaxKind::CallStmt)
                .expect("system call stmt");
            let stmt =
                CallStmt::cast(SyntaxNodeRef::new(&parsed.file, stmt_id)).expect("call stmt");
            assert_eq!(stmt.call_kind(src), Some(CallStmtKind::SystemFunction));
            assert_eq!(parsed.file.count_kind(stmt_id, SyntaxKind::Error), 0);
        }
    }

    #[test]
    fn parses_function_decl_as_block_stmt() {
        let parsed = crate::parse("FUNCTION /aif/file_process_data\n  WRITE 'x'.\nENDFUNCTION.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::FunctionDecl), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::WriteStmt), 1);
    }

    #[test]
    fn function_header_exposes_structured_sections_and_params() {
        let src = "FUNCTION /AIF/FILE_PROCESS_DATA\n  IMPORTING\n    iv_count TYPE i OPTIONAL\n    iv_ref TYPE REF TO object OPTIONAL\n  EXPORTING\n    VALUE(ev_ok) TYPE c\n  CHANGING\n    cv_text TYPE string\n  TABLES\n    it_rows LIKE sy-uname OPTIONAL\n  EXCEPTIONS\n    not_found\n    failed.\nENDFUNCTION.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let function = FunctionDecl::cast(SyntaxNodeRef::new(
            &parsed.file,
            parsed
                .file
                .find_first_kind(parsed.file.root(), SyntaxKind::FunctionDecl)
                .expect("function decl"),
        ))
        .expect("function decl");
        assert_eq!(
            function
                .name_token()
                .and_then(|name| name.name(src))
                .as_deref(),
            Some("/aif/file_process_data")
        );
        let sections = function.param_sections().collect::<Vec<_>>();
        assert_eq!(sections.len(), 5);
        assert_eq!(
            sections[0].kind(src),
            Some(FunctionParamSectionKind::Importing)
        );
        assert_eq!(
            sections[1].kind(src),
            Some(FunctionParamSectionKind::Exporting)
        );
        assert_eq!(
            sections[2].kind(src),
            Some(FunctionParamSectionKind::Changing)
        );
        assert_eq!(
            sections[3].kind(src),
            Some(FunctionParamSectionKind::Tables)
        );
        assert_eq!(
            sections[4].kind(src),
            Some(FunctionParamSectionKind::Exceptions)
        );
        let exporting_param = sections[1].params().next().expect("exporting param");
        assert_eq!(
            exporting_param
                .name_token()
                .and_then(|name| name.name(src))
                .as_deref(),
            Some("ev_ok")
        );
        assert_eq!(
            exporting_param.passing_kind(src),
            FormParamPassingKind::Value
        );
        let importing_params: Vec<_> = sections[0].params().collect();
        assert!(importing_params[0].is_optional(src));
        assert!(!importing_params[0].has_default_value(src));
        assert!(importing_params[1].is_optional(src));
        assert!(!importing_params[1].has_default_value(src));
        assert_eq!(
            parsed
                .file
                .count_kind(function.syntax().id(), SyntaxKind::FunctionParamSection),
            5
        );
        assert_eq!(
            parsed
                .file
                .count_kind(function.syntax().id(), SyntaxKind::FunctionParam),
            7
        );
    }

    #[test]
    fn function_header_exposes_inline_exceptions_entries() {
        let src = "FUNCTION z_demo EXCEPTIONS not_found failed. ENDFUNCTION.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let function = FunctionDecl::cast(SyntaxNodeRef::new(
            &parsed.file,
            parsed
                .file
                .find_first_kind(parsed.file.root(), SyntaxKind::FunctionDecl)
                .expect("function decl"),
        ))
        .expect("function decl");
        let section = function
            .param_sections()
            .next()
            .expect("exceptions section");
        assert_eq!(
            section.kind(src),
            Some(FunctionParamSectionKind::Exceptions)
        );
        let names = section
            .params()
            .filter_map(|param| param.name_token().and_then(|name| name.name(src)))
            .collect::<Vec<_>>();
        assert_eq!(
            names.iter().map(|name| name.as_ref()).collect::<Vec<_>>(),
            vec!["not_found", "failed"]
        );
    }

    #[test]
    fn function_header_exposes_raising_section_and_exception_type_refs() {
        let src = "FUNCTION z_demo IMPORTING iv_count TYPE i RAISING resumable(/sttp/cx_demo) cx_other. ENDFUNCTION.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let function = FunctionDecl::cast(SyntaxNodeRef::new(
            &parsed.file,
            parsed
                .file
                .find_first_kind(parsed.file.root(), SyntaxKind::FunctionDecl)
                .expect("function decl"),
        ))
        .expect("function decl");
        let sections = function.param_sections().collect::<Vec<_>>();
        assert_eq!(sections.len(), 2);
        assert_eq!(
            sections[1].kind(src),
            Some(FunctionParamSectionKind::Raising)
        );
        let entries = sections[1].params().collect::<Vec<_>>();
        assert_eq!(entries.len(), 2);
        assert!(entries[0].is_resumable(src));
        assert_eq!(
            entries[0]
                .type_ref()
                .and_then(|type_ref| type_ref.display_text(src)),
            Some("/sttp/cx_demo")
        );
        assert_eq!(
            entries[1]
                .type_ref()
                .and_then(|type_ref| type_ref.display_text(src)),
            Some("cx_other")
        );
    }

    #[test]
    fn function_header_param_exposes_default_value_flag() {
        let src = "FUNCTION z_demo\n  IMPORTING\n    iv_count TYPE i DEFAULT 1\n    iv_mode TYPE i OPTIONAL DEFAULT 2.\nENDFUNCTION.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let function = FunctionDecl::cast(SyntaxNodeRef::new(
            &parsed.file,
            parsed
                .file
                .find_first_kind(parsed.file.root(), SyntaxKind::FunctionDecl)
                .expect("function decl"),
        ))
        .expect("function decl");
        let importing_params: Vec<_> = function
            .param_sections()
            .next()
            .expect("importing section")
            .params()
            .collect();
        assert!(!importing_params[0].is_optional(src));
        assert!(importing_params[0].has_default_value(src));
        assert!(importing_params[1].is_optional(src));
        assert!(importing_params[1].has_default_value(src));
    }

    #[test]
    fn parses_function_named_variable_assignment_as_assignment() {
        let parsed = crate::parse("function = ucomm = end. EXIT.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::AssignStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::FunctionDecl), 0);
    }

    #[test]
    fn parses_call_function_with_tables_and_exceptions_as_structured_call_stmt() {
        let parsed = crate::parse(
            "CALL FUNCTION 'SWA_STRING_SPLIT'\n  EXPORTING\n    input_string = iv_message\n  TABLES\n    string_components = lt_strings\n  EXCEPTIONS\n    OTHERS = 1.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::CallStmt)
            .expect("call stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallArgList), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallArgSection), 3);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallNamedArg), 3);
    }

    #[test]
    fn parses_other_call_like_leads_as_call_stmt() {
        for src in [
            "CALL TRANSFORMATION id SOURCE text = lv_xml RESULT XML lv_out.",
            "CALL BADI lo_badi->run.",
            "CALL SCREEN 9000.",
            "CALL SCREEN 9000 STARTING AT 10 5 ENDING AT 40 20.",
            "CALL TRANSACTION u_tcode WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN.",
        ] {
            let parsed = crate::parse(src);
            assert!(parsed.errors.is_empty(), "{src}: {:?}", parsed.errors);
            assert_eq!(
                parsed
                    .file
                    .count_kind(parsed.file.root(), SyntaxKind::CallStmt),
                1,
                "{src}"
            );
        }
    }

    #[test]
    fn parses_call_transaction_kind() {
        let src = "CALL TRANSACTION u_tcode WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt_id = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::CallStmt)
            .expect("call transaction stmt");
        let stmt = CallStmt::cast(SyntaxNodeRef::new(&parsed.file, stmt_id)).expect("call stmt");
        assert_eq!(stmt.call_kind(src), Some(CallStmtKind::Transaction));
    }

    #[test]
    fn parses_call_transformation_with_xml_source_and_result_writer() {
        let parsed = crate::parse(
            "CALL TRANSFORMATION /sttp/json_xml_to_upper\n  SOURCE XML lv_json\n  RESULT XML lo_writer.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::CallStmt)
            .expect("call transformation stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallArgList), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallArgSection), 4);
        assert_eq!(
            parsed.file.count_kind(stmt, SyntaxKind::CallPositionalArg),
            2
        );
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_call_transformation_id_with_source_xml_and_named_result() {
        let parsed = crate::parse(
            "CALL TRANSFORMATION id\n  SOURCE XML lv_json_hex\n  RESULT result = ev_data.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::CallStmt)
            .expect("call transformation stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallArgList), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallArgSection), 3);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallNamedArg), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::Error), 0);
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
    fn rejects_call_method_inline_args_without_opening_padding() {
        for src in ["CALL METHOD lo_handler->run(iv_mode = lv_mode )."] {
            let parsed = crate::parse(src);
            assert!(
                parsed
                    .errors
                    .iter()
                    .any(|err| err.message.contains("method call arguments")),
                "{src}: {:?}",
                parsed.errors
            );
            assert_eq!(
                parsed
                    .file
                    .count_kind(parsed.file.root(), SyntaxKind::CallMethodStmt),
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
    fn accepts_call_method_inline_args_with_inner_padding() {
        let parsed = crate::parse("CALL METHOD lo_handler->run( iv_mode = lv_mode ).");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::CallMethodStmt),
            1
        );
    }

    #[test]
    fn accepts_call_method_inline_args_without_closing_padding() {
        let parsed = crate::parse("CALL METHOD lo_handler->run( iv_mode = lv_mode).");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::CallMethodStmt)
            .expect("call method stmt");
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallExpr), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::CallNamedArg), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::Error), 0);
    }

    #[test]
    fn parses_find_stmt_with_structured_operands() {
        let parsed =
            crate::parse("FIND FIRST OCCURRENCE OF | | IN iv_tag_path MATCH OFFSET lv_first_sep.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::FindStmt)
            .expect("find stmt");
        assert_eq!(
            parsed.file.count_kind(stmt, SyntaxKind::FindPatternOperand),
            1
        );
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::FindInOperand), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::FindMatchTarget), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::ExprIdent), 2);
    }

    #[test]
    fn parses_find_stmt_with_results_inline_data_target() {
        let parsed = crate::parse(
            "FIND ALL OCCURRENCES OF REGEX '\\b[A-Z0-9]+\\b' IN lv_response_string RESULTS DATA(lt_match).",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::FindStmt)
            .expect("find stmt");
        assert_eq!(
            parsed.file.count_kind(stmt, SyntaxKind::FindPatternOperand),
            1
        );
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::FindInOperand), 1);
        assert_eq!(
            parsed.file.count_kind(stmt, SyntaxKind::FindResultsTarget),
            1
        );
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::DataInlineDecl), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::ExprIdent), 1);
    }

    #[test]
    fn parses_find_stmt_with_submatches_inline_data_target() {
        let parsed = crate::parse(
            "FIND FIRST OCCURRENCE OF REGEX '<Modulus>\\s*([^<]+)\\s*</Modulus>' IN iv_key_text SUBMATCHES DATA(lv_modulus_b64).",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let stmt = parsed
            .file
            .find_first_kind(parsed.file.root(), SyntaxKind::FindStmt)
            .expect("find stmt");
        assert_eq!(
            parsed.file.count_kind(stmt, SyntaxKind::FindSubmatchTarget),
            1
        );
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::DataInlineDecl), 1);
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::ExprIdent), 1);
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

    #[test]
    fn parses_submit_stmt_with_full_documented_option_set() {
        let src = "\
SUBMIT (lv_report)
  USING SELECTION-SCREEN '1100'
  VIA SELECTION-SCREEN
  USING SELECTION-SET lv_variant
  USING SELECTION-SETS OF PROGRAM lv_prog
  WITH SELECTION-TABLE lt_rspar
  WITH p_bukrs EQ lv_bukrs
  WITH s_erdat NOT BETWEEN lv_low AND lv_high SIGN lv_sign
  WITH s_vkorg IN lt_vkorg
  WITH FREE SELECTIONS lt_texpr
  LINE-SIZE lv_width
  LINE-COUNT lv_lines
  TO SAP-SPOOL
  SPOOL PARAMETERS ls_pri
  ARCHIVE PARAMETERS ls_arc
  WITHOUT SPOOL DYNPRO
  USER lv_user
  VIA JOB lv_job NUMBER lv_count LANGUAGE lv_lang
  AND RETURN.";
        let parsed = crate::parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);

        let stmt = SubmitStmt::cast(SyntaxNodeRef::new(
            &parsed.file,
            parsed
                .file
                .find_first_kind(parsed.file.root(), SyntaxKind::SubmitStmt)
                .expect("submit stmt"),
        ))
        .expect("submit stmt");

        assert!(stmt.target().is_some());
        assert!(stmt.selection_screen().is_some());
        assert!(stmt.selection_set().is_some());
        assert!(stmt.selection_sets_program().is_some());
        assert!(stmt.selection_table().is_some());
        assert_eq!(stmt.with_clauses().count(), 3);
        assert!(stmt.free_selections().is_some());
        assert!(stmt.line_size().is_some());
        assert!(stmt.line_count().is_some());
        assert!(stmt.spool_parameters().is_some());
        assert!(stmt.archive_parameters().is_some());
        assert!(stmt.user().is_some());
        assert!(stmt.job().is_some());
        assert!(stmt.job_number().is_some());
        assert!(stmt.language().is_some());
    }

    #[test]
    fn parses_submit_stmt_exporting_list_to_memory() {
        let parsed = crate::parse("SUBMIT rsnast00 EXPORTING LIST TO MEMORY AND RETURN.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::SubmitStmt),
            1
        );
    }
}
