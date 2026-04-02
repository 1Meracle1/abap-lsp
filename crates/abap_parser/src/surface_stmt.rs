use abap_ast::arena::{NodeId, SyntaxTreeBuilder};
use abap_ast::SyntaxKind;
use abap_lexer::{Token, TokenKind};

use crate::block_helpers::{
    is_keyword, parse_body_until_keywords, parse_header_until_period, recover_skip_after_keyword,
    skip_trivia,
};
use crate::stmt_period::{
    is_definite_stmt_lead_keyword, scan_until_statement_period, token_begins_line,
    unterminated_err_end, StmtPeriodScan,
};
use crate::syntax::token_leaf;

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

fn select_header_is_flat(tokens: &[Token], source: &str, idx: usize, next_after_header: usize) -> bool {
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
                if tokens.get(j).is_some_and(|next| is_keyword(source, next, "of")) {
                    j = skip_trivia(tokens, j + 1);
                }
            }
            if tokens.get(j).is_some_and(|next| is_keyword(source, next, "table")) {
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

fn match_hyphenated_keyword(source: &str, tokens: &[Token], idx: usize, parts: &[&str]) -> Option<usize> {
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
        SyntaxKind::SimpleStmt,
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
        SyntaxKind::SimpleStmt,
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
        let node = b.branch(
            SyntaxKind::SimpleStmt,
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
    if !is_keyword(source, read_tok, "read") || !tokens.get(idx + 1).is_some_and(|t| is_keyword(source, t, "table")) {
        return None;
    }
    match scan_read_table_stmt_period(tokens, source, idx + 2) {
        StmtPeriodScan::Found(period_i) => {
            let mut children = Vec::with_capacity(period_i - idx + 1);
            for t in &tokens[idx..=period_i] {
                children.push(token_leaf(b, t));
            }
            let node =
                b.branch(SyntaxKind::ReadTableStmt, read_tok.range.start..tokens[period_i].range.end, &children);
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
    } else if let Some(next) = match_hyphenated_keyword(source, tokens, idx, &["start", "of", "selection"]) {
        next
    } else if let Some(next) = match_hyphenated_keyword(source, tokens, idx, &["end", "of", "selection"]) {
        next
    } else if let Some(next) = match_hyphenated_keyword(source, tokens, idx, &["top", "of", "page"]) {
        next
    } else if let Some(next) = match_hyphenated_keyword(source, tokens, idx, &["end", "of", "page"]) {
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
    let node = b.branch(SyntaxKind::EventBlock, start_tok.range.start..end, &children);
    Some((node, next))
}

pub fn try_parse_form_decl(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    try_parse_block_stmt(b, source, tokens, idx, "form", "ENDFORM", SyntaxKind::FormDecl, errors)
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
        let node = b.branch(SyntaxKind::SelectStmt, select_tok.range.start..end_pos, &children);
        return Some((node, next_after));
    }

    let end = children
        .last()
        .copied()
        .map(|id| b.span(id).end)
        .unwrap_or(select_tok.range.end);
    let node = b.branch(SyntaxKind::SelectStmt, select_tok.range.start..end, &children);
    Some((node, cursor))
}

#[cfg(test)]
mod tests {
    use abap_ast::SyntaxKind;

    #[test]
    fn parses_form_body() {
        let parsed = crate::parse("FORM run. DATA lv TYPE i. ENDFORM.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(parsed.file.count_kind(parsed.file.root(), SyntaxKind::FormDecl), 1);
    }

    #[test]
    fn parses_class_method_impl() {
        let parsed = crate::parse(
            "CLASS lcl IMPLEMENTATION. METHOD run. WRITE 'x'. ENDMETHOD. ENDCLASS.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(parsed.file.count_kind(parsed.file.root(), SyntaxKind::ClassDecl), 1);
        assert_eq!(parsed.file.count_kind(parsed.file.root(), SyntaxKind::MethodDecl), 1);
    }

    #[test]
    fn parses_select_endselect_block() {
        let parsed = crate::parse("SELECT * FROM t INTO wa. WRITE wa. ENDSELECT.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(parsed.file.count_kind(parsed.file.root(), SyntaxKind::SelectStmt), 1);
        assert_eq!(parsed.file.count_kind(parsed.file.root(), SyntaxKind::WriteStmt), 1);
    }

    #[test]
    fn parses_flat_select_into_table_without_endselect() {
        let parsed = crate::parse("SELECT * FROM t INTO TABLE lt_rows. WRITE 'x'.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(parsed.file.count_kind(parsed.file.root(), SyntaxKind::SelectStmt), 1);
        assert_eq!(parsed.file.count_kind(parsed.file.root(), SyntaxKind::WriteStmt), 1);
    }

    #[test]
    fn parses_flat_select_into_tuple_without_endselect() {
        let parsed = crate::parse(
            "SELECT MAX( bup_role_variant ) COUNT( * ) INTO ( lv_max, lv_count ) FROM demo. IF lv_count > 0. ENDIF.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(parsed.file.count_kind(parsed.file.root(), SyntaxKind::SelectStmt), 1);
        assert_eq!(parsed.file.count_kind(parsed.file.root(), SyntaxKind::IfStmt), 1);
    }

    #[test]
    fn parses_flat_select_count_into_scalar_without_endselect() {
        let parsed =
            crate::parse("SELECT COUNT( * ) FROM demo INTO lv_count WHERE key = value. IF lv_count > 0. ENDIF.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(parsed.file.count_kind(parsed.file.root(), SyntaxKind::SelectStmt), 1);
        assert_eq!(parsed.file.count_kind(parsed.file.root(), SyntaxKind::IfStmt), 1);
    }

    #[test]
    fn parses_multiline_call_method_with_named_args_as_one_statement() {
        let parsed = crate::parse(
            "CALL METHOD zcl_demo=>run\n  EXPORTING\n    iv_a = lv_a\n  IMPORTING\n    ev_b = lv_b.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(parsed.file.count_kind(parsed.file.root(), SyntaxKind::SimpleStmt), 1);
        assert_eq!(parsed.file.count_kind(parsed.file.root(), SyntaxKind::AssignStmt), 0);
    }

    #[test]
    fn parses_create_object_with_exporting_clause_as_one_statement() {
        let parsed = crate::parse(
            "CREATE OBJECT lo_client\n  EXPORTING\n    iv_dest = lv_dest.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(parsed.file.count_kind(parsed.file.root(), SyntaxKind::SimpleStmt), 1);
    }

    #[test]
    fn class_hyphenated_decls_do_not_start_nested_class_blocks() {
        let parsed = crate::parse(
            "CLASS lcl DEFINITION.\n  PUBLIC SECTION.\n    CLASS-DATA gv_value TYPE i.\n    CLASS-METHODS run\n      IMPORTING\n        iv_x TYPE i\n      EXPORTING\n        ev_y TYPE i.\nENDCLASS.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(parsed.file.count_kind(parsed.file.root(), SyntaxKind::ClassDecl), 1);
    }

    #[test]
    fn parses_multiline_read_table_with_key_into_inline_data() {
        let parsed = crate::parse(
            "READ TABLE lt_obj_hier_upd\n  WITH KEY gs1_es = lv_epc\n  INTO DATA(ls_ser_par).",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(parsed.file.count_kind(parsed.file.root(), SyntaxKind::ReadTableStmt), 1);
    }

    #[test]
    fn parses_multiline_read_table_with_assigning_field_symbol() {
        let parsed = crate::parse(
            "READ TABLE lt_unpack_lvls\n  WITH KEY parent = ls_ser_par-gs1_es_parent\n  ASSIGNING FIELD-SYMBOL(<fs_unpack_data>).",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(parsed.file.count_kind(parsed.file.root(), SyntaxKind::ReadTableStmt), 1);
    }

    #[test]
    fn parses_multiline_read_table_transporting_no_fields() {
        let parsed = crate::parse(
            "READ TABLE <fs_unpack_data>-children\n  WITH KEY table_line = lv_epc\n  TRANSPORTING NO FIELDS.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(parsed.file.count_kind(parsed.file.root(), SyntaxKind::ReadTableStmt), 1);
    }

    #[test]
    fn parses_read_table_index_using_key_assigning() {
        let parsed = crate::parse(
            "READ TABLE itab INDEX idx USING KEY sort_key ASSIGNING FIELD-SYMBOL(<fs>).",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(parsed.file.count_kind(parsed.file.root(), SyntaxKind::ReadTableStmt), 1);
    }

    #[test]
    fn parses_select_where_condition_split_after_and() {
        let parsed = crate::parse(
            "SELECT *\n  APPENDING CORRESPONDING FIELDS OF TABLE lt_rows\n  FROM demo\n  WHERE bupid = ls_key-bupid AND\n        regid = ls_key-regid.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(parsed.file.count_kind(parsed.file.root(), SyntaxKind::SelectStmt), 1);
    }

    #[test]
    fn parses_direct_static_method_call_with_named_args_as_simple_stmt() {
        let parsed = crate::parse(
            "cl_abap_message_digest=>calculate_hash_for_char(\n  EXPORTING\n    if_algorithm = lv_algorithm\n    if_data      = lv_data\n  IMPORTING\n    ef_hashstring = lv_hashstring\n).",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(parsed.file.count_kind(parsed.file.root(), SyntaxKind::SimpleStmt), 1);
        assert_eq!(parsed.file.count_kind(parsed.file.root(), SyntaxKind::AssignStmt), 0);
    }

    #[test]
    fn parses_raise_exception_type_with_exporting_named_args() {
        let parsed = crate::parse(
            "RAISE EXCEPTION TYPE /sttp/cx_base_exception\n  EXPORTING\n    message_text = gv_dummy_msg\n    returncode   = /sttp/cl_constants=>gcs_rc-fail.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(parsed.file.count_kind(parsed.file.root(), SyntaxKind::SimpleStmt), 1);
        assert_eq!(parsed.file.count_kind(parsed.file.root(), SyntaxKind::AssignStmt), 0);
    }

    #[test]
    fn parses_endat_as_simple_stmt() {
        let parsed = crate::parse("ENDAT.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(parsed.file.count_kind(parsed.file.root(), SyntaxKind::SimpleStmt), 1);
    }

    #[test]
    fn class_definition_load_does_not_start_class_block() {
        let parsed = crate::parse(
            "CLASS lcl DEFINITION.\n  PUBLIC SECTION.\n    CLASS cl_demo DEFINITION LOAD.\nENDCLASS.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert_eq!(parsed.file.count_kind(parsed.file.root(), SyntaxKind::ClassDecl), 1);
    }
}
