//! Arithmetic and string-concat expression parsing matching `legacy/src/lang/parser/parser.odin`:
//! `parse_concat_expr` → `parse_additive_expr` → `parse_multiplicative_expr` → `parse_unary_expr` → operand / atom.
//!
//! [`parse_logical_expr`] follows `legacy` `parse_or_expr` → `parse_and_expr` → `parse_not_expr` →
//! `parse_comparison_expr` (operands are concat-exprs). Parentheses after a gap use [`parse_logical_expr`]
//! inside when parsing conditions (Odin `parse_expr` / `parse_paren_expr`).
//! Comparisons cover symbolic and `EQ`…`GE`/`CO`…`NP`/`IN`/`BETWEEN`; [`SyntaxKind::IsPredicate`] for
//! `IS [NOT] INITIAL|…`; [`SyntaxKind::InstanceOfPredicate`] for `IS [NOT] INSTANCE OF type` (type =
//! concat-expr). Comment tokens (including lexer `##…` pragmas) are skipped inside the expression parser.

use crate::syntax::parse_char_string_template;
use abap_ast::SyntaxKind;
use abap_ast::arena::{NodeId, SyntaxTreeBuilder};
use abap_lexer::{Token, TokenKind, have_space_between};

fn token_leaf(b: &mut SyntaxTreeBuilder, token: &Token) -> NodeId {
    b.leaf(SyntaxKind::Token, token.range.clone())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ParenInner {
    /// `( … )` groups concatenation / arithmetic only (assignment RHS, comparison operands).
    Concat,
    /// `( … )` groups full logical expressions (IF / WHILE conditions).
    Logical,
}

struct Parser<'a, 'b> {
    source: &'a str,
    tokens: &'a [Token],
    idx: usize,
    prev: &'a Token,
    b: &'b mut SyntaxTreeBuilder,
    paren_inner: ParenInner,
}

#[inline]
fn ident_eq(source: &str, t: &Token, kw: &str) -> bool {
    t.kind == TokenKind::Ident && t.lexeme(source).eq_ignore_ascii_case(kw)
}

fn is_comparison_op(source: &str, t: &Token) -> bool {
    match t.kind {
        TokenKind::Lt
        | TokenKind::Gt
        | TokenKind::Le
        | TokenKind::Ge
        | TokenKind::Ne
        | TokenKind::Eq => true,
        TokenKind::Ident => {
            let s = t.lexeme(source);
            s.eq_ignore_ascii_case("EQ")
                || s.eq_ignore_ascii_case("NE")
                || s.eq_ignore_ascii_case("LT")
                || s.eq_ignore_ascii_case("LE")
                || s.eq_ignore_ascii_case("GT")
                || s.eq_ignore_ascii_case("GE")
                || s.eq_ignore_ascii_case("CO")
                || s.eq_ignore_ascii_case("CN")
                || s.eq_ignore_ascii_case("CA")
                || s.eq_ignore_ascii_case("NA")
                || s.eq_ignore_ascii_case("CS")
                || s.eq_ignore_ascii_case("NS")
                || s.eq_ignore_ascii_case("CP")
                || s.eq_ignore_ascii_case("NP")
                || s.eq_ignore_ascii_case("IN")
                || s.eq_ignore_ascii_case("BETWEEN")
        }
        _ => false,
    }
}

impl<'a, 'b> Parser<'a, 'b> {
    /// Comments include ABAP pragmas (`##...`) from the lexer.
    fn skip_trivia(&mut self) {
        while self.idx < self.tokens.len() && self.tokens[self.idx].kind == TokenKind::Comment {
            self.prev = &self.tokens[self.idx];
            self.idx += 1;
        }
    }

    fn curr(&mut self) -> Option<&'a Token> {
        self.skip_trivia();
        self.tokens.get(self.idx)
    }

    fn bump(&mut self) -> Option<&'a Token> {
        self.skip_trivia();
        let t = self.tokens.get(self.idx)?;
        self.prev = t;
        self.idx += 1;
        Some(t)
    }

    fn parse_concat_expr(&mut self) -> Option<NodeId> {
        let mut expr = self.parse_additive_expr()?;

        loop {
            let Some(op_tok) = self.curr() else {
                break;
            };
            if op_tok.kind != TokenKind::Ampersand {
                break;
            }

            let saved_idx = self.idx;
            let saved_prev = self.prev;

            self.bump(); // &
            let right = match self.parse_additive_expr() {
                Some(e) => e,
                None if self.curr().map(|t| t.kind) == Some(TokenKind::Ampersand) => {
                    self.bump();
                    self.parse_additive_expr()?
                }
                None => {
                    self.idx = saved_idx;
                    self.prev = saved_prev;
                    break;
                }
            };

            let r = self.b.span(expr).start..self.b.span(right).end;
            let op = token_leaf(self.b, op_tok);
            expr = self.b.branch(SyntaxKind::BinaryExpr, r, &[expr, op, right]);
        }

        Some(expr)
    }

    fn parse_additive_expr(&mut self) -> Option<NodeId> {
        let mut expr = self.parse_multiplicative_expr()?;

        loop {
            let Some(curr) = self.curr() else {
                break;
            };

            let is_plus = curr.kind == TokenKind::Plus;
            let is_minus_binary =
                curr.kind == TokenKind::Minus && have_space_between(self.prev, curr);

            if !is_plus && !is_minus_binary {
                break;
            }

            let op_tok = self.bump()?;
            let right = self.parse_multiplicative_expr()?;
            let r = self.b.span(expr).start..self.b.span(right).end;
            let op = token_leaf(self.b, op_tok);
            expr = self.b.branch(SyntaxKind::BinaryExpr, r, &[expr, op, right]);
        }

        Some(expr)
    }

    fn parse_multiplicative_expr(&mut self) -> Option<NodeId> {
        let mut expr = self.parse_unary_expr()?;

        loop {
            let Some(curr) = self.curr() else {
                break;
            };

            let is_star_slash = curr.kind == TokenKind::Star || curr.kind == TokenKind::Slash;
            let is_mod_div = curr.kind == TokenKind::Ident
                && (curr.lexeme(self.source).eq_ignore_ascii_case("mod")
                    || curr.lexeme(self.source).eq_ignore_ascii_case("div"));

            if !is_star_slash && !is_mod_div {
                break;
            }

            let op_tok = self.bump()?;
            let right = self.parse_unary_expr()?;
            let r = self.b.span(expr).start..self.b.span(right).end;
            let op = token_leaf(self.b, op_tok);
            expr = self.b.branch(SyntaxKind::BinaryExpr, r, &[expr, op, right]);
        }

        Some(expr)
    }

    fn parse_unary_expr(&mut self) -> Option<NodeId> {
        let curr = self.curr()?;
        if curr.kind == TokenKind::Plus || curr.kind == TokenKind::Minus {
            let op_tok = self.bump()?;
            let inner = self.parse_unary_expr()?;
            let r = op_tok.range.start..self.b.span(inner).end;
            let op = token_leaf(self.b, op_tok);
            return Some(self.b.branch(SyntaxKind::UnaryExpr, r, &[op, inner]));
        }

        let operand = self.parse_operand()?;
        self.parse_atom_expr(operand)
    }

    fn parse_atom_expr(&mut self, mut value: NodeId) -> Option<NodeId> {
        loop {
            let Some(curr) = self.curr() else {
                break;
            };

            let is_selector = matches!(
                curr.kind,
                TokenKind::Arrow | TokenKind::FatArrow | TokenKind::Tilde
            ) || (curr.kind == TokenKind::Minus
                && !have_space_between(self.prev, curr));
            if is_selector {
                let op_tok = self.bump()?;
                let field_tok = self.curr()?;
                if field_tok.kind != TokenKind::Ident
                    && !(op_tok.kind == TokenKind::Arrow && field_tok.kind == TokenKind::Star)
                {
                    break;
                }
                let field_tok = self.bump()?;
                let op = token_leaf(self.b, op_tok);
                let field_leaf = token_leaf(self.b, field_tok);
                let field = self.b.branch(
                    SyntaxKind::ExprIdent,
                    field_tok.range.clone(),
                    &[field_leaf],
                );
                let range = self.b.span(value).start..self.b.span(field).end;
                value = self
                    .b
                    .branch(SyntaxKind::SelectorExpr, range, &[value, op, field]);
                continue;
            }

            if let Some(substring) = self.try_parse_substring_expr(value) {
                value = substring;
                continue;
            }

            if curr.kind == TokenKind::LParen && !have_space_between(self.prev, curr) {
                value = self.parse_call_expr(value)?;
                continue;
            }

            break;
        }
        Some(value)
    }

    fn try_parse_substring_expr(&mut self, base: NodeId) -> Option<NodeId> {
        if !self.node_can_start_substring(base) {
            return None;
        }

        let saved_idx = self.idx;
        let saved_prev = self.prev;

        if let Some(expr) = self.try_parse_substring_with_offset(base) {
            return Some(expr);
        }
        self.idx = saved_idx;
        self.prev = saved_prev;

        if let Some(expr) = self.try_parse_substring_without_offset(base) {
            return Some(expr);
        }
        self.idx = saved_idx;
        self.prev = saved_prev;
        None
    }

    fn try_parse_substring_with_offset(&mut self, base: NodeId) -> Option<NodeId> {
        let plus = self.curr()?;
        if plus.kind != TokenKind::Plus || have_space_between(self.prev, plus) {
            return None;
        }

        let plus_tok = self.bump()?;
        let offset_start = self.idx;
        let lparen_idx = self.find_tight_lparen_for_substring(offset_start)?;
        let offset =
            self.parse_complete_concat_expr(&self.tokens[offset_start..lparen_idx], plus_tok)?;
        let (lparen, length, rparen, next_idx) =
            self.parse_substring_length_group_at(lparen_idx)?;

        self.idx = next_idx;
        self.prev = rparen;

        let plus_leaf = token_leaf(self.b, plus_tok);
        let lparen_leaf = token_leaf(self.b, lparen);
        let rparen_leaf = token_leaf(self.b, rparen);
        let range = self.b.span(base).start..rparen.range.end;
        Some(self.b.branch(
            SyntaxKind::SubstringExpr,
            range,
            &[base, plus_leaf, offset, lparen_leaf, length, rparen_leaf],
        ))
    }

    fn try_parse_substring_without_offset(&mut self, base: NodeId) -> Option<NodeId> {
        let lparen = self.curr()?;
        if lparen.kind != TokenKind::LParen || have_space_between(self.prev, lparen) {
            return None;
        }

        let lparen_idx = self.idx;
        let (lparen, length, rparen, next_idx) =
            self.parse_substring_length_group_at(lparen_idx)?;
        self.idx = next_idx;
        self.prev = rparen;

        let lparen_leaf = token_leaf(self.b, lparen);
        let rparen_leaf = token_leaf(self.b, rparen);
        let range = self.b.span(base).start..rparen.range.end;
        Some(self.b.branch(
            SyntaxKind::SubstringExpr,
            range,
            &[base, lparen_leaf, length, rparen_leaf],
        ))
    }

    fn parse_complete_concat_expr(
        &mut self,
        tokens: &'a [Token],
        prev_before_first: &'a Token,
    ) -> Option<NodeId> {
        if tokens.is_empty() {
            return None;
        }

        let mut nested = Parser {
            source: self.source,
            tokens,
            idx: 0,
            prev: prev_before_first,
            b: self.b,
            paren_inner: ParenInner::Concat,
        };
        let expr = nested.parse_concat_expr()?;
        nested.skip_trivia();
        if nested.idx != tokens.len() {
            return None;
        }
        Some(expr)
    }

    fn parse_substring_length_group_at(
        &mut self,
        lparen_idx: usize,
    ) -> Option<(&'a Token, NodeId, &'a Token, usize)> {
        let lparen = self.tokens.get(lparen_idx)?;
        if lparen.kind != TokenKind::LParen {
            return None;
        }
        let rparen_idx = self.find_matching_paren_from(lparen_idx)?;
        let rparen = self.tokens.get(rparen_idx)?;
        let length =
            self.parse_complete_concat_expr(&self.tokens[lparen_idx + 1..rparen_idx], lparen)?;
        Some((lparen, length, rparen, rparen_idx + 1))
    }

    fn find_tight_lparen_for_substring(&self, start_idx: usize) -> Option<usize> {
        let mut idx = start_idx;
        while idx < self.tokens.len() {
            let token = &self.tokens[idx];
            if token.kind == TokenKind::LParen
                && idx > start_idx
                && !have_space_between(&self.tokens[idx - 1], token)
            {
                return Some(idx);
            }
            if matches!(
                token.kind,
                TokenKind::Period
                    | TokenKind::Comma
                    | TokenKind::Eq
                    | TokenKind::QuestionEq
                    | TokenKind::RParen
            ) {
                return None;
            }
            idx += 1;
        }
        None
    }

    fn find_matching_paren_from(&self, start_idx: usize) -> Option<usize> {
        let mut depth = 0i32;
        for (idx, tok) in self.tokens.iter().enumerate().skip(start_idx) {
            match tok.kind {
                TokenKind::LParen => depth += 1,
                TokenKind::RParen => {
                    depth -= 1;
                    if depth == 0 {
                        return Some(idx);
                    }
                }
                _ => {}
            }
        }
        None
    }

    fn call_padding_is_valid(&self, lparen_idx: usize, rparen_idx: usize) -> bool {
        let lparen = &self.tokens[lparen_idx];
        let rparen = &self.tokens[rparen_idx];
        let inner: Vec<_> = self.tokens[lparen_idx + 1..rparen_idx]
            .iter()
            .filter(|token| token.kind != TokenKind::Comment)
            .collect();
        match (inner.first(), inner.last()) {
            (Some(first), Some(last)) => {
                have_space_between(lparen, first) && have_space_between(last, rparen)
            }
            _ => have_space_between(lparen, rparen),
        }
    }

    fn node_can_start_substring(&self, node: NodeId) -> bool {
        let span = self.b.span(node);
        let mut saw_ident = false;

        for token in self
            .tokens
            .iter()
            .filter(|token| token.range.start >= span.start && token.range.end <= span.end)
        {
            match token.kind {
                TokenKind::Ident => saw_ident = true,
                TokenKind::Minus => {}
                TokenKind::Comment => {}
                TokenKind::Arrow
                | TokenKind::FatArrow
                | TokenKind::Tilde
                | TokenKind::LParen
                | TokenKind::RParen
                | TokenKind::LBracket
                | TokenKind::RBracket
                | TokenKind::LBrace
                | TokenKind::RBrace => return false,
                _ => return false,
            }
        }

        saw_ident
    }

    fn parse_balanced_token_group(&mut self) -> Option<Vec<NodeId>> {
        let lparen = self.bump()?;
        debug_assert_eq!(lparen.kind, TokenKind::LParen);
        let mut children = vec![token_leaf(self.b, lparen)];
        let mut depth = 1i32;
        while self.idx < self.tokens.len() {
            let tok = self.bump()?;
            match tok.kind {
                TokenKind::LParen => depth += 1,
                TokenKind::RParen => {
                    depth -= 1;
                    children.push(token_leaf(self.b, tok));
                    if depth == 0 {
                        return Some(children);
                    }
                    continue;
                }
                _ => {}
            }
            children.push(token_leaf(self.b, tok));
        }
        None
    }

    fn parse_call_expr(&mut self, callee: NodeId) -> Option<NodeId> {
        let lparen_idx = self.idx;
        let rparen_idx = self.find_matching_paren_from(lparen_idx)?;
        let mut children = vec![callee];
        let extra = self.parse_balanced_token_group()?;
        children.extend(extra);
        let range = self.b.span(callee).start..self.b.span(*children.last().unwrap()).end;
        let kind = if self.call_padding_is_valid(lparen_idx, rparen_idx) {
            SyntaxKind::CallExpr
        } else {
            SyntaxKind::Error
        };
        Some(self.b.branch(kind, range, &children))
    }

    fn parse_constructor_expr(&mut self) -> Option<NodeId> {
        let kw_tok = self.bump()?;
        let mut children = vec![token_leaf(self.b, kw_tok)];

        while let Some(curr) = self.curr() {
            if curr.kind == TokenKind::LParen {
                let mut group = self.parse_balanced_token_group()?;
                children.append(&mut group);
                let range = kw_tok.range.start..self.b.span(*children.last().unwrap()).end;
                return Some(self.b.branch(SyntaxKind::ConstructorExpr, range, &children));
            }
            if matches!(
                curr.kind,
                TokenKind::Ident
                    | TokenKind::Hash
                    | TokenKind::Arrow
                    | TokenKind::FatArrow
                    | TokenKind::Minus
                    | TokenKind::Tilde
            ) {
                let tok = self.bump()?;
                children.push(token_leaf(self.b, tok));
                continue;
            }
            break;
        }

        let range = kw_tok.range.start..self.b.span(*children.last().unwrap()).end;
        Some(self.b.branch(SyntaxKind::ConstructorExpr, range, &children))
    }

    fn parse_paren_expr(&mut self) -> Option<NodeId> {
        let lparen = self.bump()?;
        debug_assert_eq!(lparen.kind, TokenKind::LParen);
        let inner = match self.paren_inner {
            ParenInner::Concat => self.parse_concat_expr()?,
            ParenInner::Logical => self.parse_or_expr()?,
        };
        let rparen = self.curr()?;
        if rparen.kind != TokenKind::RParen {
            return None;
        }
        let rparen = self.bump()?;
        let r = lparen.range.start..rparen.range.end;
        let a = token_leaf(self.b, lparen);
        let c = token_leaf(self.b, rparen);
        Some(self.b.branch(SyntaxKind::ParenExpr, r, &[a, inner, c]))
    }

    fn parse_or_expr(&mut self) -> Option<NodeId> {
        let mut left = self.parse_and_expr()?;
        loop {
            let Some(curr) = self.curr() else {
                break;
            };
            if !ident_eq(self.source, curr, "OR") {
                break;
            }
            let op_tok = self.bump()?;
            let right = self.parse_and_expr()?;
            let r = self.b.span(left).start..self.b.span(right).end;
            let op = token_leaf(self.b, op_tok);
            left = self.b.branch(SyntaxKind::BinaryExpr, r, &[left, op, right]);
        }
        Some(left)
    }

    fn parse_and_expr(&mut self) -> Option<NodeId> {
        let mut left = self.parse_not_expr()?;
        loop {
            let Some(curr) = self.curr() else {
                break;
            };
            if !ident_eq(self.source, curr, "AND") {
                break;
            }
            let op_tok = self.bump()?;
            let right = self.parse_not_expr()?;
            let r = self.b.span(left).start..self.b.span(right).end;
            let op = token_leaf(self.b, op_tok);
            left = self.b.branch(SyntaxKind::BinaryExpr, r, &[left, op, right]);
        }
        Some(left)
    }

    fn parse_not_expr(&mut self) -> Option<NodeId> {
        let curr = self.curr()?;
        if ident_eq(self.source, curr, "NOT") {
            let op_tok = self.bump()?;
            let inner = self.parse_not_expr()?;
            let r = op_tok.range.start..self.b.span(inner).end;
            let op = token_leaf(self.b, op_tok);
            return Some(self.b.branch(SyntaxKind::UnaryExpr, r, &[op, inner]));
        }
        self.parse_comparison_expr()
    }

    fn parse_comparison_expr(&mut self) -> Option<NodeId> {
        let left = self.parse_concat_expr()?;
        let Some(curr) = self.curr() else {
            return Some(left);
        };
        if ident_eq(self.source, curr, "IS") {
            return self.parse_is_predicate(left);
        }
        if !is_comparison_op(self.source, curr) {
            return Some(left);
        }
        let op_tok = self.bump()?;
        if ident_eq(self.source, op_tok, "BETWEEN") {
            return self.parse_between_expr(left, op_tok);
        }
        let right = self.parse_concat_expr()?;
        let r = self.b.span(left).start..self.b.span(right).end;
        let op = token_leaf(self.b, op_tok);
        Some(self.b.branch(SyntaxKind::BinaryExpr, r, &[left, op, right]))
    }

    /// `IS [NOT] predicate…` after `expr` (subject already parsed).
    fn parse_is_predicate(&mut self, subject: NodeId) -> Option<NodeId> {
        let is_tok = self.bump()?;
        if !ident_eq(self.source, is_tok, "IS") {
            return None;
        }
        let start = self.b.span(subject).start;
        let is_leaf = token_leaf(self.b, is_tok);
        let mut children = vec![subject, is_leaf];

        if self.curr().is_some_and(|t| ident_eq(self.source, t, "NOT")) {
            let n = self.bump()?;
            children.push(token_leaf(self.b, n));
        }

        let Some(pred_kw) = self.curr() else {
            let end = self.b.span(*children.last().unwrap()).end;
            return Some(self.b.branch(SyntaxKind::Error, start..end, &children));
        };
        if ident_eq(self.source, pred_kw, "INSTANCE") {
            let i_tok = self.bump()?;
            children.push(token_leaf(self.b, i_tok));
            let of_kw = self.curr()?;
            if !ident_eq(self.source, of_kw, "OF") {
                let bad = self.bump()?;
                children.push(token_leaf(self.b, bad));
                let end = self.b.span(*children.last().unwrap()).end;
                return Some(self.b.branch(SyntaxKind::Error, start..end, &children));
            }
            let of_tok = self.bump()?;
            children.push(token_leaf(self.b, of_tok));
            let ty = self.parse_concat_expr()?;
            children.push(ty);
            let end = self.b.span(ty).end;
            return Some(
                self.b
                    .branch(SyntaxKind::InstanceOfPredicate, start..end, &children),
            );
        }

        let s = pred_kw.lexeme(self.source);
        let simple = s.eq_ignore_ascii_case("INITIAL")
            || s.eq_ignore_ascii_case("BOUND")
            || s.eq_ignore_ascii_case("ASSIGNED")
            || s.eq_ignore_ascii_case("REQUESTED")
            || s.eq_ignore_ascii_case("SUPPLIED");
        if simple {
            let t = self.bump()?;
            children.push(token_leaf(self.b, t));
            let end = self.b.span(*children.last().unwrap()).end;
            return Some(
                self.b
                    .branch(SyntaxKind::IsPredicate, start..end, &children),
            );
        }

        let t = self.bump()?;
        children.push(token_leaf(self.b, t));
        let end = self.b.span(*children.last().unwrap()).end;
        Some(self.b.branch(SyntaxKind::Error, start..end, &children))
    }

    fn parse_between_expr(&mut self, left: NodeId, between_tok: &'a Token) -> Option<NodeId> {
        let low = self.parse_concat_expr()?;
        let Some(and_kw) = self.curr() else {
            let r = self.b.span(left).start..self.b.span(low).end;
            let bt = token_leaf(self.b, between_tok);
            return Some(self.b.branch(SyntaxKind::Error, r, &[left, bt, low]));
        };
        if !ident_eq(self.source, and_kw, "AND") {
            let bad = self.bump()?;
            let bad_leaf = token_leaf(self.b, bad);
            let r = self.b.span(left).start..bad.range.end;
            let bt = token_leaf(self.b, between_tok);
            return Some(
                self.b
                    .branch(SyntaxKind::Error, r, &[left, bt, low, bad_leaf]),
            );
        }
        let and_tok = self.bump()?;
        let high = self.parse_concat_expr()?;
        let r = self.b.span(left).start..self.b.span(high).end;
        let bt = token_leaf(self.b, between_tok);
        let at = token_leaf(self.b, and_tok);
        Some(
            self.b
                .branch(SyntaxKind::BetweenExpr, r, &[left, bt, low, at, high]),
        )
    }

    fn parse_operand(&mut self) -> Option<NodeId> {
        let curr = self.curr()?;
        match curr.kind {
            TokenKind::StringTemplate => {
                let start = self.idx;
                let (node, next) =
                    parse_char_string_template(self.source, self.tokens, start, self.b);
                self.idx = next;
                self.prev = &self.tokens[next.saturating_sub(1)];
                Some(node)
            }
            TokenKind::Ident => {
                let is_constructor_keyword = matches!(
                    curr.lexeme(self.source).to_ascii_uppercase().as_str(),
                    "NEW"
                        | "VALUE"
                        | "CONV"
                        | "REF"
                        | "CAST"
                        | "EXACT"
                        | "CORRESPONDING"
                        | "FILTER"
                        | "REDUCE"
                        | "SWITCH"
                        | "COND"
                );
                if is_constructor_keyword {
                    return self.parse_constructor_expr();
                }
                let t = self.bump()?;
                let leaf = token_leaf(self.b, t);
                Some(
                    self.b
                        .branch(SyntaxKind::ExprIdent, t.range.clone(), &[leaf]),
                )
            }
            TokenKind::Number | TokenKind::String => {
                let t = self.bump()?;
                let leaf = token_leaf(self.b, t);
                Some(
                    self.b
                        .branch(SyntaxKind::ExprLiteral, t.range.clone(), &[leaf]),
                )
            }
            TokenKind::LParen if have_space_between(self.prev, curr) => self.parse_paren_expr(),
            _ => None,
        }
    }
}

/// Parses `tokens` as one expression using the same precedence as Odin's `parse_concat_expr` chain.
///
/// `prev_before_first` should be the real token before `tokens[0]` when available (e.g. `{` in
/// a template interpolation, or `=` on the RHS of an assignment). This mirrors Odin's `p.prev_tok`
/// so that:
/// - `( ... )` grouping is recognized only with a gap before `(` (not a method call `(`, which is absent here),
/// - binary `-` requires `have_space_between` the previous token and `-`; unary `-` does not.
pub fn parse_arithmetic_expr(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    prev_before_first: Option<&Token>,
) -> NodeId {
    if tokens.is_empty() {
        return b.branch(SyntaxKind::TemplateExpr, 0..0, &[]);
    }

    let sentinel = Token {
        kind: TokenKind::Other,
        range: tokens[0].range.start..tokens[0].range.start,
    };
    let initial_prev = prev_before_first.unwrap_or(&sentinel);

    let mut p = Parser {
        source,
        tokens,
        idx: 0,
        prev: initial_prev,
        b,
        paren_inner: ParenInner::Concat,
    };

    let expr = p.parse_concat_expr();
    let expr = expr.unwrap_or_else(|| {
        let r = tokens[0].range.start..tokens[0].range.end;
        let t0 = token_leaf(p.b, &tokens[0]);
        p.b.branch(SyntaxKind::Error, r, &[t0])
    });

    if p.idx < tokens.len() {
        let mut children = vec![expr];
        for t in &tokens[p.idx..] {
            children.push(token_leaf(p.b, t));
        }
        let r = p.b.span(*children.first().unwrap()).start..p.b.span(*children.last().unwrap()).end;
        return p.b.branch(SyntaxKind::TemplateExpr, r, &children);
    }

    let r = p.b.span(expr);
    p.b.branch(SyntaxKind::TemplateExpr, r, &[expr])
}

/// Parses a logical / relational condition (`IF`, `ELSEIF`, `WHILE`, …) using Odin `parse_logical_expr` rules.
pub fn parse_logical_expr(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    prev_before_first: Option<&Token>,
) -> NodeId {
    if tokens.is_empty() {
        return b.branch(SyntaxKind::TemplateExpr, 0..0, &[]);
    }

    let sentinel = Token {
        kind: TokenKind::Other,
        range: tokens[0].range.start..tokens[0].range.start,
    };
    let initial_prev = prev_before_first.unwrap_or(&sentinel);

    let mut p = Parser {
        source,
        tokens,
        idx: 0,
        prev: initial_prev,
        b,
        paren_inner: ParenInner::Logical,
    };

    let expr = p.parse_or_expr();
    let expr = expr.unwrap_or_else(|| {
        let r = tokens[0].range.start..tokens[0].range.end;
        let t0 = token_leaf(p.b, &tokens[0]);
        p.b.branch(SyntaxKind::Error, r, &[t0])
    });

    if p.idx < tokens.len() {
        let mut children = vec![expr];
        for t in &tokens[p.idx..] {
            children.push(token_leaf(p.b, t));
        }
        let r = p.b.span(*children.first().unwrap()).start..p.b.span(*children.last().unwrap()).end;
        return p.b.branch(SyntaxKind::TemplateExpr, r, &children);
    }

    let r = p.b.span(expr);
    p.b.branch(SyntaxKind::TemplateExpr, r, &[expr])
}

#[cfg(test)]
mod tests {
    use super::*;
    use abap_ast::arena::SyntaxTree;
    use abap_lexer::tokenize;

    /// Tokens after first `=`, stopping before statement-ending `.`.
    fn expr_tokens_after_eq(src: &str) -> (String, Vec<Token>, Token) {
        let r = tokenize(src);
        let eq_i = r
            .tokens
            .iter()
            .position(|t| t.kind == TokenKind::Eq)
            .expect("need =");
        let prev = r.tokens[eq_i].clone();
        let mut i = eq_i + 1;
        while i < r.tokens.len() && r.tokens[i].kind == TokenKind::Comment {
            i += 1;
        }
        let mut body: Vec<Token> = Vec::new();
        for t in r.tokens[i..].iter() {
            if t.kind == TokenKind::Period {
                break;
            }
            if t.kind == TokenKind::Eof {
                break;
            }
            body.push(t.clone());
        }
        (src.to_string(), body, prev)
    }

    fn bin_op_lexeme<'a>(source: &'a str, tree: &SyntaxTree, bin: NodeId) -> &'a str {
        let op = tree.children(bin).nth(1).expect("op slot");
        let t = tree.children(op).next().unwrap_or(op);
        let r = tree.range(t);
        source.get(r).unwrap_or("")
    }

    fn outer_binary(tree: &SyntaxTree, root: NodeId) -> NodeId {
        let kinds = [
            SyntaxKind::BinaryExpr,
            SyntaxKind::UnaryExpr,
            SyntaxKind::ParenExpr,
            SyntaxKind::SelectorExpr,
            SyntaxKind::SubstringExpr,
            SyntaxKind::CallExpr,
            SyntaxKind::ConstructorExpr,
            SyntaxKind::IsPredicate,
            SyntaxKind::InstanceOfPredicate,
            SyntaxKind::BetweenExpr,
        ];
        let wrap = tree
            .children(root)
            .find(|&c| kinds.contains(&tree.kind(c)))
            .unwrap_or(root);
        match tree.kind(wrap) {
            SyntaxKind::TemplateExpr => tree
                .children(wrap)
                .find(|&c| kinds.contains(&tree.kind(c)))
                .expect("inner expr"),
            _ => wrap,
        }
    }

    #[test]
    fn precedence_a_plus_b_times_c() {
        let (src, tokens, prev) = expr_tokens_after_eq("DATA(result) = a + b * c.");
        let mut b = SyntaxTreeBuilder::default();
        let root = parse_arithmetic_expr(&mut b, &src, &tokens, Some(&prev));
        let tree = b.finish(root);
        let bnode = outer_binary(&tree, root);
        assert_eq!(tree.kind(bnode), SyntaxKind::BinaryExpr);
        assert_eq!(bin_op_lexeme(&src, &tree, bnode), "+");
        let right = tree.children(bnode).nth(2).expect("rhs");
        assert_eq!(tree.kind(right), SyntaxKind::BinaryExpr);
        assert_eq!(bin_op_lexeme(&src, &tree, right), "*");
    }

    #[test]
    fn left_associative_subtraction() {
        let (src, tokens, prev) = expr_tokens_after_eq("DATA(result) = a - b - c.");
        let mut b = SyntaxTreeBuilder::default();
        let root = parse_arithmetic_expr(&mut b, &src, &tokens, Some(&prev));
        let tree = b.finish(root);
        let bnode = outer_binary(&tree, root);
        assert_eq!(bin_op_lexeme(&src, &tree, bnode), "-");
        assert_eq!(
            tree.kind(tree.children(bnode).nth(2).unwrap()),
            SyntaxKind::ExprIdent
        );
        let left = tree.children(bnode).next().unwrap();
        assert_eq!(tree.kind(left), SyntaxKind::BinaryExpr);
        assert_eq!(bin_op_lexeme(&src, &tree, left), "-");
    }

    #[test]
    fn mod_and_div_are_multiplicative() {
        let (src, tokens, prev) = expr_tokens_after_eq("DATA(r) = a + b MOD c DIV d.");
        let mut b = SyntaxTreeBuilder::default();
        let root = parse_arithmetic_expr(&mut b, &src, &tokens, Some(&prev));
        let tree = b.finish(root);
        let top = outer_binary(&tree, root);
        assert_eq!(bin_op_lexeme(&src, &tree, top), "+");
        let right = tree.children(top).nth(2).unwrap();
        assert_eq!(tree.kind(right), SyntaxKind::BinaryExpr);
        assert_eq!(bin_op_lexeme(&src, &tree, right), "DIV");
        let div_left = tree.children(right).next().unwrap();
        assert_eq!(tree.kind(div_left), SyntaxKind::BinaryExpr);
        assert_eq!(bin_op_lexeme(&src, &tree, div_left), "MOD");
    }

    #[test]
    fn unary_minus_binds_tighter_than_multiply() {
        let (src, tokens, prev) = expr_tokens_after_eq("DATA(r) = a * - b.");
        let mut b = SyntaxTreeBuilder::default();
        let root = parse_arithmetic_expr(&mut b, &src, &tokens, Some(&prev));
        let tree = b.finish(root);
        let star = outer_binary(&tree, root);
        assert_eq!(bin_op_lexeme(&src, &tree, star), "*");
        let r = tree.children(star).nth(2).unwrap();
        assert_eq!(tree.kind(r), SyntaxKind::UnaryExpr);
    }

    #[test]
    fn string_concat_is_looser_than_plus() {
        let (src, tokens, prev) = expr_tokens_after_eq("DATA(r) = a + b & c.");
        let mut b = SyntaxTreeBuilder::default();
        let root = parse_arithmetic_expr(&mut b, &src, &tokens, Some(&prev));
        let tree = b.finish(root);
        let amp = outer_binary(&tree, root);
        assert_eq!(bin_op_lexeme(&src, &tree, amp), "&");
        let l = tree.children(amp).next().unwrap();
        assert_eq!(tree.kind(l), SyntaxKind::BinaryExpr);
        assert_eq!(bin_op_lexeme(&src, &tree, l), "+");
    }

    #[test]
    fn parenthesized_addition_before_multiply() {
        let (src, tokens, prev) = expr_tokens_after_eq("DATA(result) = ( a + b ) * c.");
        let mut b = SyntaxTreeBuilder::default();
        let root = parse_arithmetic_expr(&mut b, &src, &tokens, Some(&prev));
        let tree = b.finish(root);
        let star = outer_binary(&tree, root);
        assert_eq!(bin_op_lexeme(&src, &tree, star), "*");
        let paren = tree.children(star).next().unwrap();
        assert_eq!(tree.kind(paren), SyntaxKind::ParenExpr);
        let inner = tree.children(paren).nth(1).unwrap();
        assert_eq!(tree.kind(inner), SyntaxKind::BinaryExpr);
        assert_eq!(bin_op_lexeme(&src, &tree, inner), "+");
    }

    #[test]
    fn logical_or_binds_looser_than_and() {
        let src = "a = 1 OR b = 2 AND c = 3";
        let tok = tokenize(src);
        let mut b = SyntaxTreeBuilder::default();
        let root = parse_logical_expr(&mut b, src, &tok.tokens, None);
        let tree = b.finish(root);
        let outer = outer_binary(&tree, root);
        assert_eq!(bin_op_lexeme(src, &tree, outer), "OR");
        let right = tree.children(outer).nth(2).unwrap();
        assert_eq!(tree.kind(right), SyntaxKind::BinaryExpr);
        assert_eq!(bin_op_lexeme(src, &tree, right), "AND");
    }

    #[test]
    fn logical_is_initial_not_bound_and_pragma() {
        // Use `sy_subrc`: `sy-subrc` tokenizes as `sy` `-` `subrc` and is not one operand here.
        let src = "##NEEDED lv IS NOT INITIAL AND sy_subrc IS BOUND";
        let tok = tokenize(src);
        let mut b = SyntaxTreeBuilder::default();
        let root = parse_logical_expr(&mut b, src, &tok.tokens, None);
        let tree = b.finish(root);
        let outer = outer_binary(&tree, root);
        assert_eq!(tree.kind(outer), SyntaxKind::BinaryExpr);
        assert_eq!(bin_op_lexeme(src, &tree, outer), "AND");
        assert!(tree.count_kind(root, SyntaxKind::IsPredicate) >= 2);
    }

    #[test]
    fn between_in_condition() {
        let src = "lv BETWEEN 1 AND max_v";
        let tok = tokenize(src);
        let mut b = SyntaxTreeBuilder::default();
        let root = parse_logical_expr(&mut b, src, &tok.tokens, None);
        let tree = b.finish(root);
        assert_eq!(tree.count_kind(root, SyntaxKind::BetweenExpr), 1);
    }

    #[test]
    fn in_and_co_comparisons() {
        let src = "a IN itab AND mask CN pattern";
        let tok = tokenize(src);
        let mut b = SyntaxTreeBuilder::default();
        let root = parse_logical_expr(&mut b, src, &tok.tokens, None);
        let tree = b.finish(root);
        let top = outer_binary(&tree, root);
        assert_eq!(bin_op_lexeme(src, &tree, top), "AND");
        let left = tree.children(top).next().unwrap();
        let right = tree.children(top).nth(2).unwrap();
        assert_eq!(tree.kind(left), SyntaxKind::BinaryExpr);
        assert_eq!(bin_op_lexeme(src, &tree, left), "IN");
        assert_eq!(tree.kind(right), SyntaxKind::BinaryExpr);
        assert_eq!(bin_op_lexeme(src, &tree, right), "CN");
    }

    #[test]
    fn instance_of_predicate() {
        let src = "oref IS INSTANCE OF cl_foo";
        let tok = tokenize(src);
        let mut b = SyntaxTreeBuilder::default();
        let root = parse_logical_expr(&mut b, src, &tok.tokens, None);
        let tree = b.finish(root);
        assert_eq!(tree.count_kind(root, SyntaxKind::InstanceOfPredicate), 1);
    }

    #[test]
    fn selector_and_call_expr_on_assignment_rhs() {
        let parsed = crate::parse("lv = lo_obj->run( iv_x = 1 ).");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SelectorExpr), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::CallExpr), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn call_expr_requires_padding_inside_parentheses() {
        for src in [
            "lv = lo_prog->add_statement(lo_assign).",
            "lv = lo_prog->add_statement( lo_assign).",
            "lv = lo_prog->add_statement(lo_assign ).",
        ] {
            let parsed = crate::parse(src);
            let root = parsed.file.root();
            assert_eq!(
                parsed.file.count_kind(root, SyntaxKind::CallExpr),
                0,
                "{src}"
            );
            assert!(
                parsed.file.count_kind(root, SyntaxKind::Error) >= 1,
                "{src}"
            );
        }
    }

    #[test]
    fn selector_deref_expr_on_assignment_rhs() {
        let parsed = crate::parse("lv_name = lr_row->*-name.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SelectorExpr), 2);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn substring_length_on_assignment_rhs() {
        let parsed = crate::parse("lv_text = ls_time(14).");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SubstringExpr), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::CallExpr), 0);
    }

    #[test]
    fn substring_offset_and_length_on_assignment_rhs() {
        let parsed = crate::parse("lv_text = ls_time+2(8).");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SubstringExpr), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::CallExpr), 0);
    }

    #[test]
    fn constructor_expr_inside_inline_data() {
        let parsed = crate::parse("DATA(lo_obj) = NEW zcl_demo( ).");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::DataInlineDecl), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::ConstructorExpr), 1);
    }

    #[test]
    fn assignment_rhs_template_builds_semantic_expression_nodes() {
        let parsed = crate::parse(
            "rv_text = |({ mo_left->to_string( ) } { mv_op } { mo_right->to_string( ) })|.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::CharStringTemplate),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::TemplateInterpolation),
            3
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::SelectorExpr), 2);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::CallExpr), 2);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn template_interpolation_can_contain_nested_template_operand() {
        let parsed = crate::parse("rv_text = |prefix { |{ mv_inner }| } suffix|.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::CharStringTemplate),
            2
        );
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::TemplateInterpolation),
            2
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }
}
