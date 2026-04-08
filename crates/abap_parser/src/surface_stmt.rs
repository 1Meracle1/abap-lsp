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
    is_named_arg_clause_keyword, scan_until_statement_period, token_begins_line,
    unterminated_err_end,
};
use crate::syntax::token_leaf;
use crate::type_ref::build_type_ref_node;

#[derive(Clone, Copy)]
enum EventBlockLead {
    Single(&'static str),
    Hyphenated(&'static [&'static str]),
}

const EVENT_BLOCK_LEADS: &[EventBlockLead] = &[
    EventBlockLead::Single("initialization"),
    EventBlockLead::Hyphenated(&["start", "of", "selection"]),
    EventBlockLead::Hyphenated(&["end", "of", "selection"]),
    EventBlockLead::Hyphenated(&["top", "of", "page"]),
    EventBlockLead::Hyphenated(&["end", "of", "page"]),
];

const EVENT_BLOCK_BODY_BOUNDARY_KEYWORDS: &[&str] = &[
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
];

const GET_TIME_STAMP_FIELD_LEAD: &[&str] = &["get", "time", "stamp", "field"];
const GET_REFERENCE_OF_LEAD: &[&str] = &["get", "reference", "of"];

#[derive(Clone, Copy, PartialEq, Eq)]
enum CallLikeLeadKind {
    CallMethod,
    CallStmt,
    CreateObject,
    CreateData,
}

const CALL_LIKE_LEADS: &[(&[&str], CallLikeLeadKind)] = &[
    (&["call", "method"], CallLikeLeadKind::CallMethod),
    (&["call", "function"], CallLikeLeadKind::CallStmt),
    (&["call", "transformation"], CallLikeLeadKind::CallStmt),
    (&["call", "badi"], CallLikeLeadKind::CallStmt),
    (&["create", "object"], CallLikeLeadKind::CreateObject),
    (&["create", "data"], CallLikeLeadKind::CreateData),
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
        (Some(first), Some(last)) => {
            have_space_between(lparen, first) && have_space_between(last, rparen)
        }
        _ => have_space_between(lparen, rparen),
    }
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
                if i > idx + 2 && !have_space_between(&tokens[i - 1], token) {
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
            || is_keyword(source, token, "up")
            || is_keyword(source, token, "union")
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
        if let Some((inline_decl, next_idx)) =
            try_parse_data_inline_decl(b, source, tokens, expr_start)
            && next_idx == target_end
        {
            children.push(inline_decl);
        } else if let Some((inline_decl, next_idx)) =
            try_parse_field_symbol_inline_decl(b, source, tokens, expr_start)
            && next_idx == target_end
        {
            children.push(inline_decl);
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

#[derive(Clone, Copy, PartialEq, Eq)]
enum SelectClauseKind {
    Distinct,
    UpTo,
    From,
    Into,
    Appending,
    Where,
    GroupBy,
    Having,
    OrderBy,
    ForAllEntries,
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
        push_token_children(b, &mut children, tokens, start, as_idx + 1);
        let alias_idx = skip_trivia(tokens, as_idx + 1);
        if let Some(alias_node) =
            build_token_branch(b, SyntaxKind::SqlAlias, tokens, alias_idx, alias_idx + 1)
        {
            children.push(alias_node);
        }
        push_token_children(b, &mut children, tokens, alias_idx + 1, end_exclusive);
    } else {
        push_token_children(b, &mut children, tokens, start, end_exclusive);
    }
    let range = tokens[start].range.start..tokens[end_exclusive - 1].range.end;
    Some(b.branch(SyntaxKind::SqlDataSource, range, &children))
}

fn build_sql_predicate_branch(
    b: &mut SyntaxTreeBuilder,
    kind: SyntaxKind,
    _source: &str,
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
        } else if let Some(predicate_node) = build_token_branch(
            b,
            SyntaxKind::SqlPredicateExpr,
            tokens,
            predicate_start,
            end_exclusive,
        ) {
            children.push(predicate_node);
        }
    } else if let Some(predicate_node) = build_token_branch(
        b,
        SyntaxKind::SqlPredicateExpr,
        tokens,
        predicate_start,
        end_exclusive,
    ) {
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

fn build_projection_value_node(
    b: &mut SyntaxTreeBuilder,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> Option<NodeId> {
    if start >= end_exclusive {
        return None;
    }
    if end_exclusive == start + 1 && tokens[start].kind == TokenKind::Star {
        return build_token_branch(b, SyntaxKind::SqlStar, tokens, start, end_exclusive);
    }
    if end_exclusive == start + 3
        && tokens[start].kind == TokenKind::Ident
        && tokens[start + 1].kind == TokenKind::Tilde
        && tokens[start + 2].kind == TokenKind::Star
    {
        return build_token_branch(
            b,
            SyntaxKind::SqlQualifiedStar,
            tokens,
            start,
            end_exclusive,
        );
    }
    if end_exclusive == start + 1 && tokens[start].kind == TokenKind::Ident {
        return build_token_branch(b, SyntaxKind::SqlColumnRef, tokens, start, end_exclusive);
    }
    if end_exclusive == start + 3
        && tokens[start].kind == TokenKind::Ident
        && tokens[start + 1].kind == TokenKind::Tilde
        && tokens[start + 2].kind == TokenKind::Ident
    {
        return build_token_branch(b, SyntaxKind::SqlColumnRef, tokens, start, end_exclusive);
    }
    None
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
    if let Some(value_node) = build_projection_value_node(b, tokens, start, alias_start) {
        children.push(value_node);
    } else {
        push_token_children(b, &mut children, tokens, start, alias_start);
    }
    if alias_start < end_exclusive {
        let alias_idx = skip_trivia(tokens, alias_start + 1);
        push_token_children(b, &mut children, tokens, alias_start, alias_idx);
        if let Some(alias_node) =
            build_token_branch(b, SyntaxKind::SqlAlias, tokens, alias_idx, alias_idx + 1)
        {
            children.push(alias_node);
        }
        push_token_children(b, &mut children, tokens, alias_idx + 1, end_exclusive);
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
    let mut item_start = start;
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
                if let Some(item) = build_sql_projection_item(b, source, tokens, item_start, idx) {
                    children.push(item);
                }
                item_start = idx + 1;
            }
            _ => {}
        }
        idx += 1;
    }
    if let Some(item) = build_sql_projection_item(b, source, tokens, item_start, end_exclusive) {
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
        SelectClauseKind::UpTo => build_token_branch(
            b,
            SyntaxKind::SelectUpToClause,
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
    }
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
                let clause_end = if kind == SelectClauseKind::Distinct {
                    skip_trivia(tokens, cursor + 1)
                } else {
                    scan_until_clause(tokens, cursor + 1, period_i, |tokens, idx| {
                        select_clause_start_kind(source, tokens, idx).is_some()
                    })
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
                if t.kind == TokenKind::Ident
                    && token_begins_line(source, t)
                    && is_definite_stmt_lead_keyword(source, t)
                    && !(is_sql_case_start
                        || is_sql_case_branch && sql_case_depth > 0
                        || is_sql_case_end && sql_case_depth > 0)
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
    if let Some((inline_decl, next_idx)) = try_parse_data_inline_decl(b, source, tokens, start)
        && next_idx == end_exclusive
    {
        children.push(inline_decl);
        return;
    }
    if let Some((inline_decl, next_idx)) =
        try_parse_field_symbol_inline_decl(b, source, tokens, start)
        && next_idx == end_exclusive
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
        }
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
                if matches!(tokens[i].kind, TokenKind::Slash | TokenKind::Comma) {
                    children.push(token_leaf(b, &tokens[i]));
                    i += 1;
                    continue;
                }
                let expr_end = scan_until_clause(tokens, i, period_i, |tokens, at| {
                    matches!(tokens[at].kind, TokenKind::Slash | TokenKind::Comma)
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
            let Some(into_idx) =
                find_top_level_keyword_index(source, tokens, idx + 1, period_i, "into")
            else {
                let raw = token_children(b, tokens, idx, period_i + 1);
                let node = b.branch(
                    SyntaxKind::Error,
                    concat_tok.range.start..tokens[period_i].range.end,
                    &raw,
                );
                return (node, period_i + 1);
            };

            let mut i = idx + 1;
            while i < into_idx {
                let end_idx = consume_concatenate_operand(source, tokens, i, into_idx, &["into"]);
                if end_idx == i {
                    i += 1;
                    continue;
                }
                push_expr_child(
                    b,
                    &mut children,
                    source,
                    tokens,
                    i,
                    end_idx,
                    Some(concat_tok),
                );
                i = end_idx;
            }

            children.push(token_leaf(b, &tokens[into_idx]));
            let target_end = consume_concatenate_operand(
                source,
                tokens,
                into_idx + 1,
                period_i,
                &["separated", "respecting", "in"],
            );
            push_expr_child(
                b,
                &mut children,
                source,
                tokens,
                into_idx + 1,
                target_end,
                Some(&tokens[into_idx]),
            );
            i = target_end;

            while i < period_i {
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
                        period_i,
                        &["respecting", "in"],
                    );
                    push_expr_child(
                        b,
                        &mut children,
                        source,
                        tokens,
                        sep_start,
                        sep_end,
                        Some(&tokens[i + 1]),
                    );
                    i = sep_end;
                    continue;
                }
                children.push(token_leaf(b, token));
                i += 1;
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
            let Some(at_idx) =
                find_top_level_keyword_index(source, tokens, idx + 1, period_i, "at")
            else {
                let raw = token_children(b, tokens, idx, period_i + 1);
                let node = b.branch(
                    SyntaxKind::Error,
                    split_tok.range.start..tokens[period_i].range.end,
                    &raw,
                );
                return (node, period_i + 1);
            };
            let Some(into_idx) =
                find_top_level_keyword_index(source, tokens, at_idx + 1, period_i, "into")
            else {
                let raw = token_children(b, tokens, idx, period_i + 1);
                let node = b.branch(
                    SyntaxKind::Error,
                    split_tok.range.start..tokens[period_i].range.end,
                    &raw,
                );
                return (node, period_i + 1);
            };

            push_expr_child(
                b,
                &mut children,
                source,
                tokens,
                idx + 1,
                at_idx,
                Some(split_tok),
            );
            children.push(token_leaf(b, &tokens[at_idx]));

            let separator_end =
                consume_concatenate_operand(source, tokens, at_idx + 1, into_idx, &["into"]);
            push_expr_child(
                b,
                &mut children,
                source,
                tokens,
                at_idx + 1,
                separator_end,
                Some(&tokens[at_idx]),
            );

            children.push(token_leaf(b, &tokens[into_idx]));
            let mut i = separator_end.max(into_idx + 1);
            while i < period_i {
                let token = &tokens[i];
                if is_keyword(source, token, "in") {
                    push_token_children(b, &mut children, tokens, i, period_i);
                    break;
                }
                let end_idx = consume_concatenate_operand(source, tokens, i, period_i, &["in"]);
                if end_idx == i {
                    children.push(token_leaf(b, token));
                    i += 1;
                    continue;
                }
                push_expr_child(
                    b,
                    &mut children,
                    source,
                    tokens,
                    i,
                    end_idx,
                    Some(if i == into_idx + 1 {
                        &tokens[into_idx]
                    } else {
                        &tokens[i - 1]
                    }),
                );
                i = end_idx;
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

pub fn try_parse_message_stmt(
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
        SyntaxKind::MessageStmt,
        "message",
        errors,
        "syntax error: expected '.' after MESSAGE statement",
    )
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
    parse_simple_keyword_stmt(
        b,
        source,
        tokens,
        idx,
        SyntaxKind::FindStmt,
        "find",
        errors,
        "syntax error: expected '.' after FIND",
    )
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
            CallLikeLeadKind::CallStmt => SyntaxKind::CallStmt,
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
            CallLikeLeadKind::CallStmt => {
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
            let clause_starts =
                |tokens: &[Token], idx: usize| read_table_clause_starts(source, tokens, idx);
            let mut children = Vec::with_capacity(period_i - idx + 1);
            children.push(token_leaf(b, read_tok));
            children.push(token_leaf(b, &tokens[idx + 1]));

            let mut i = idx + 2;
            let source_end = scan_and_push_expr_clause(
                b,
                &mut children,
                source,
                tokens,
                i,
                period_i,
                tokens.get(idx + 1),
                &clause_starts,
            );
            i = source_end;

            while i < period_i {
                let token = &tokens[i];
                if is_keyword(source, token, "into") {
                    children.push(token_leaf(b, token));
                    let target_start = skip_trivia(tokens, i + 1);
                    let target_end =
                        scan_until_clause(tokens, target_start, period_i, &clause_starts);
                    if let Some((inline_decl, next_idx)) =
                        try_parse_data_inline_decl(b, source, tokens, target_start)
                        && skip_trivia(tokens, next_idx) == target_end
                    {
                        children.push(inline_decl);
                        i = target_end;
                    } else {
                        push_expr_child(
                            b,
                            &mut children,
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
                        &mut children,
                        source,
                        tokens,
                        i + 1,
                        period_i,
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
                        &mut children,
                        source,
                        tokens,
                        i + 1,
                        period_i,
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
                if is_keyword(source, token, "with") {
                    children.push(token_leaf(b, token));
                    i += 1;
                    while i < period_i {
                        let current = &tokens[i];
                        if clause_starts(tokens, i) {
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
            let into_idx = find_top_level_keyword_index(source, tokens, idx + 1, period_i, "into")?;
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
    Some(parse_stmt_with_period_scan(
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
            let no_split = |_: &[Token], _: usize| false;

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
                let mut tail = scan_and_push_expr_clause(
                    b,
                    &mut children,
                    source,
                    tokens,
                    by_idx + 1,
                    period_i,
                    Some(&tokens[by_idx]),
                    &no_split,
                );
                while tail < period_i {
                    children.push(token_leaf(b, &tokens[tail]));
                    tail += 1;
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
                stmt_kind,
                delete_tok.range.start..tokens[period_i].range.end,
                &children,
            );
            (node, period_i + 1)
        },
    ))
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

            children.push(token_leaf(b, &tokens[to_idx]));
            if let Some((inline_decl, next_i)) =
                try_parse_field_symbol_inline_decl(b, source, tokens, to_idx + 1)
                && skip_trivia(tokens, next_i) == period_i
            {
                children.push(inline_decl);
            } else {
                push_expr_child(
                    b,
                    &mut children,
                    source,
                    tokens,
                    to_idx + 1,
                    period_i,
                    Some(&tokens[to_idx]),
                );
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
    if statement_starts_interface_load_stmt(source, tokens, idx) {
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

fn statement_starts_interface_load_stmt(source: &str, tokens: &[Token], idx: usize) -> bool {
    let period_idx = match scan_until_statement_period(tokens, source, idx) {
        StmtPeriodScan::Found(period_idx) => period_idx,
        StmtPeriodScan::Unterminated { .. } => return false,
    };
    let keywords = tokens[idx..period_idx]
        .iter()
        .filter(|token| token.kind == TokenKind::Ident)
        .map(|token| token.lexeme(source))
        .collect::<Vec<_>>();
    keywords.len() >= 3
        && keywords[0].eq_ignore_ascii_case("interface")
        && keywords[keywords.len() - 1].eq_ignore_ascii_case("load")
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
    fn parses_all_supported_event_block_leads() {
        let parsed = crate::parse(
            "INITIALIZATION.\nWRITE 'a'.\n\
START-OF-SELECTION.\nWRITE 'b'.\n\
END-OF-SELECTION.\nWRITE 'c'.\n\
TOP-OF-PAGE.\nWRITE 'd'.\n\
END-OF-PAGE.\nWRITE 'e'.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::EventBlock), 5);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::WriteStmt), 5);
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
        assert!(parsed.file.count_kind(root, SyntaxKind::CallExpr) >= 1);
        assert!(parsed.file.count_kind(root, SyntaxKind::SelectorExpr) >= 1);
        assert!(parsed.file.count_kind(root, SyntaxKind::ExprIdent) >= 3);
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
        assert_eq!(parsed.file.count_kind(stmt, SyntaxKind::TemplateExpr), 1);
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
    fn rejects_call_method_inline_args_without_inner_padding() {
        for src in [
            "CALL METHOD lo_handler->run(iv_mode = lv_mode ).",
            "CALL METHOD lo_handler->run( iv_mode = lv_mode).",
        ] {
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
