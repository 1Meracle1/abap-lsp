//! Build a structural syntax tree from flat tokens (string templates, interpolation, format specs).

use abap_ast::arena::{NodeId, SyntaxTree, SyntaxTreeBuilder};
use abap_ast::SyntaxKind;
use abap_lexer::{Token, TokenKind};

use crate::block_helpers::ensure_forward_progress;
use crate::expr::parse_arithmetic_expr;

/// Formatting option keywords allowed before `=` in `{ expr WIDTH = 8 … }` (ABAP string templates).
const TEMPLATE_FORMAT_NAMES: &[&str] = &[
    "WIDTH",
    "ALIGN",
    "DECIMALS",
    "ALPHA",
    "TIMESTAMP",
    "DATE",
    "TIME",
];

#[inline]
fn is_template_format_name(name: &str) -> bool {
    TEMPLATE_FORMAT_NAMES
        .iter()
        .any(|&k| k.eq_ignore_ascii_case(name))
}

pub(crate) fn token_leaf(b: &mut SyntaxTreeBuilder, token: &Token) -> NodeId {
    b.leaf(SyntaxKind::Token, token.range.clone())
}

pub fn build_file_tree(
    source: &str,
    tokens: &[Token],
    end: usize,
    errors: &mut Vec<crate::ParseError>,
) -> SyntaxTree {
    let mut b = SyntaxTreeBuilder::default();
    let mut idx = 0;
    let mut children: Vec<NodeId> = Vec::new();

    while idx < tokens.len() {
        let t = &tokens[idx];
        if t.kind == TokenKind::Eof {
            break;
        }

        let (node, next) = crate::parse_file_level_item(&mut b, source, tokens, idx, errors);
        children.push(node);
        idx = ensure_forward_progress(tokens, idx, next);
    }

    let root = b.branch(SyntaxKind::File, 0..end, &children);
    b.finish(root)
}

pub(crate) fn parse_char_string_template(
    source: &str,
    tokens: &[Token],
    start: usize,
    b: &mut SyntaxTreeBuilder,
) -> (NodeId, usize) {
    debug_assert_eq!(tokens[start].kind, TokenKind::StringTemplate);
    let mut i = start + 1;
    let mut parts: Vec<NodeId> = Vec::new();
    parts.push(token_leaf(b, &tokens[start]));

    while i < tokens.len() {
        match tokens[i].kind {
            TokenKind::Eof => break,
            TokenKind::StringTemplateLit => {
                let idx = i;
                let lit = token_leaf(b, &tokens[idx]);
                parts.push(b.branch(
                    SyntaxKind::TemplateLiteral,
                    tokens[idx].range.clone(),
                    &[lit],
                ));
                i += 1;
            }
            TokenKind::LBrace => {
                let (node, j) = parse_template_interpolation(source, tokens, i, b);
                parts.push(node);
                i = j;
            }
            TokenKind::StringTemplate => {
                parts.push(token_leaf(b, &tokens[i]));
                let range = tokens[start].range.start..tokens[i].range.end;
                let node = b.branch(SyntaxKind::CharStringTemplate, range, &parts);
                return (node, i + 1);
            }
            _ => {
                parts.push(token_leaf(b, &tokens[i]));
                i += 1;
            }
        }
    }

    let range = tokens[start].range.start..tokens.get(i).map_or(tokens[start].range.end, |t| t.range.start);
    let node = b.branch(SyntaxKind::CharStringTemplate, range, &parts);
    (node, i)
}

fn parse_template_interpolation(
    source: &str,
    tokens: &[Token],
    start: usize,
    b: &mut SyntaxTreeBuilder,
) -> (NodeId, usize) {
    debug_assert_eq!(tokens[start].kind, TokenKind::LBrace);
    let mut depth = 1usize;
    let mut i = start + 1;
    while i < tokens.len() {
        match tokens[i].kind {
            TokenKind::LBrace => depth += 1,
            TokenKind::RBrace => {
                depth -= 1;
                if depth == 0 {
                    let body = &tokens[start + 1..i];
                    let full_range = tokens[start].range.start..tokens[i].range.end;
                    let (expr, specs) = split_interpolation_body(source, body, &tokens[start], b);

                    let mut child_ids: Vec<NodeId> = Vec::with_capacity(3 + specs.len());
                    child_ids.push(token_leaf(b, &tokens[start]));
                    child_ids.push(expr);
                    child_ids.extend(specs);
                    child_ids.push(token_leaf(b, &tokens[i]));

                    let node = b.branch(SyntaxKind::TemplateInterpolation, full_range, &child_ids);
                    return (node, i + 1);
                }
            }
            _ => {}
        }
        i += 1;
    }

    let l = token_leaf(b, &tokens[start]);
    let node = b.branch(
        SyntaxKind::TemplateInterpolation,
        tokens[start].range.clone(),
        &[l],
    );
    (node, i)
}

fn split_interpolation_body(
    source: &str,
    body: &[Token],
    open_brace: &Token,
    b: &mut SyntaxTreeBuilder,
) -> (NodeId, Vec<NodeId>) {
    let mut idx = 0usize;
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let mut expr_end = body.len();

    while idx < body.len() {
        let t = &body[idx];
        match t.kind {
            TokenKind::LParen => paren += 1,
            TokenKind::RParen => paren -= 1,
            TokenKind::LBracket => bracket += 1,
            TokenKind::RBracket => bracket -= 1,
            TokenKind::LBrace => brace += 1,
            TokenKind::RBrace => brace -= 1,
            TokenKind::Ident if paren == 0 && bracket == 0 && brace == 0 => {
                if idx + 1 < body.len() && body[idx + 1].kind == TokenKind::Eq {
                    let name = body[idx].lexeme(source);
                    if is_template_format_name(name) {
                        expr_end = idx;
                        break;
                    }
                }
            }
            _ => {}
        }
        idx += 1;
    }

    let expr_tokens = &body[..expr_end];
    let expr_node = if expr_tokens.is_empty() {
        let r = body
            .first()
            .map(|t| t.range.start..t.range.start)
            .unwrap_or(0..0);
        b.branch(SyntaxKind::TemplateExpr, r, &[])
    } else {
        parse_arithmetic_expr(b, source, expr_tokens, Some(open_brace))
    };

    let mut specs = Vec::new();
    let mut j = expr_end;
    while j < body.len() {
        if body[j].kind != TokenKind::Ident || !is_template_format_name(body[j].lexeme(source)) {
            break;
        }
        if j + 2 >= body.len() || body[j + 1].kind != TokenKind::Eq {
            break;
        }
        let name_i = j;
        let eq_i = j + 1;
        let val_i = j + 2;
        if body[val_i].kind != TokenKind::Ident && body[val_i].kind != TokenKind::Number {
            break;
        }
        let spec_range = body[name_i].range.start..body[val_i].range.end;
        let a = token_leaf(b, &body[name_i]);
        let e = token_leaf(b, &body[eq_i]);
        let v = token_leaf(b, &body[val_i]);
        specs.push(b.branch(
            SyntaxKind::TemplateFormatSpec,
            spec_range,
            &[a, e, v],
        ));
        j = val_i + 1;
    }

    (expr_node, specs)
}

#[cfg(test)]
mod tests {
    use super::*;
    use abap_lexer::tokenize;

    fn tree_ok(src: &str) -> SyntaxTree {
        let tok = tokenize(src);
        let mut err = Vec::new();
        let file = build_file_tree(src, &tok.tokens, src.len(), &mut err);
        assert!(err.is_empty(), "{:?}", err);
        file
    }

    #[test]
    fn template_interpolation_parsing_extracts_format_specs() {
        let src = "|Amount: { lv_amount DECIMALS = 2 WIDTH = 12 } EUR|";
        let file = tree_ok(src);
        let interp = file
            .find_first_kind(file.root(), SyntaxKind::TemplateInterpolation)
            .expect("interpolation");
        assert_eq!(
            file.count_kind(interp, SyntaxKind::TemplateFormatSpec),
            2
        );
    }

    #[test]
    fn template_with_alpha_date_time() {
        let src =
            "|Material: { lv_matnr ALPHA = IN }| && |Date: { lv_date DATE = USER }| && |Time: { sy-uzeit TIME = ISO }|";
        let file = tree_ok(src);
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::CharStringTemplate),
            3
        );
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::TemplateFormatSpec),
            3
        );
    }

    #[test]
    fn default_formatting_only_expr() {
        let src = "|{ lv_amount }|";
        let file = tree_ok(src);
        let interp = file
            .find_first_kind(file.root(), SyntaxKind::TemplateInterpolation)
            .unwrap();
        assert_eq!(file.count_kind(interp, SyntaxKind::TemplateFormatSpec), 0);
    }

    #[test]
    fn template_interpolation_expr_respects_arithmetic_precedence() {
        let src = "|{ a + b * c }|";
        let file = tree_ok(src);
        let interp = file
            .find_first_kind(file.root(), SyntaxKind::TemplateInterpolation)
            .expect("interp");
        let tmpl = file
            .find_first_kind(interp, SyntaxKind::TemplateExpr)
            .expect("TemplateExpr");
        let root = file
            .child_by_kind(tmpl, SyntaxKind::BinaryExpr)
            .expect("binary at root");
        let op = file.children(root).nth(1).unwrap();
        assert_eq!(
            file.range(op),
            src.find('+').map(|s| s..s + 1).unwrap()
        );
        assert_eq!(
            file.kind(file.children(root).nth(2).unwrap()),
            SyntaxKind::BinaryExpr
        );
    }
}
