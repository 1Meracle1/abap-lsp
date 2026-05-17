//! `IF cond. ... [ELSEIF cond. ...]* [ELSE. ...] ENDIF.` (classic ABAP).

use abap_ast::SyntaxKind;
use abap_ast::arena::NodeId;
use abap_lexer::TokenKind;

use crate::parser::{PResult, Parser};

const IF_BOUNDARY_KEYWORDS: &[&str] = &["ELSEIF", "ELSE", "ENDIF"];
const ELSE_BOUNDARY_KEYWORDS: &[&str] = &["ENDIF"];

fn parse_elseif_clause_result(cursor: &mut Parser<'_, '_>) -> PResult<NodeId> {
    let fallback = cursor.current_range();
    let mut children = Vec::new();

    children.push(cursor.expect_keyword_result("ELSEIF")?);
    children.push(cursor.expect_condition_result("ELSEIF")?);
    children.push(cursor.expect_token_after_result(TokenKind::Period, "ELSEIF condition")?);
    children.extend(cursor.parse_stmt_list_until(IF_BOUNDARY_KEYWORDS));

    Ok(cursor.branch_from_children(SyntaxKind::ElseifClause, &children, fallback))
}

fn parse_else_clause_result(cursor: &mut Parser<'_, '_>) -> PResult<NodeId> {
    let fallback = cursor.current_range();
    let mut children = Vec::new();

    children.push(cursor.expect_keyword_result("ELSE")?);
    children.push(cursor.expect_token_after_result(TokenKind::Period, "ELSE")?);
    children.extend(cursor.parse_stmt_list_until(ELSE_BOUNDARY_KEYWORDS));

    Ok(cursor.branch_from_children(SyntaxKind::ElseClause, &children, fallback))
}

pub(crate) fn parse_if_stmt_result(cursor: &mut Parser<'_, '_>) -> PResult<NodeId> {
    cursor.skip_trivia();
    let fallback = cursor.current_range();
    let mut children = Vec::new();

    children.push(cursor.expect_keyword_result("IF")?);
    children.push(cursor.expect_condition_result("IF")?);
    children.push(cursor.expect_token_after_result(TokenKind::Period, "IF condition")?);

    children.extend(cursor.parse_stmt_list_until(IF_BOUNDARY_KEYWORDS));
    loop {
        cursor.skip_trivia();
        if !cursor.at_keyword("ELSEIF") {
            break;
        }
        children.push(parse_elseif_clause_result(cursor)?);
    }

    cursor.skip_trivia();
    if cursor.at_keyword("ELSE") {
        children.push(parse_else_clause_result(cursor)?);
    }

    children.push(cursor.expect_keyword_result("ENDIF")?);
    children.push(cursor.expect_token_after_result(TokenKind::Period, "ENDIF")?);

    Ok(cursor.branch_from_children(SyntaxKind::IfStmt, &children, fallback))
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
    fn missing_period_after_if_condition_invalidates_if_and_leaves_body_token() {
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
        assert!(
            parsed
                .errors
                .iter()
                .any(|e| e.message.contains("unexpected ENDIF without matching IF")),
            "{:?}",
            parsed.errors
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::IfStmt),
            0
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::AssignStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::InvalidStmt),
            1
        );
    }

    #[test]
    fn missing_period_after_elseif_condition_invalidates_if_without_scanning_arms() {
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
            0
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::ElseifClause),
            0
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::ElseClause),
            0
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::AssignStmt),
            2
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::InvalidStmt),
            1
        );
    }

    #[test]
    fn malformed_else_invalidates_if_and_preserves_following_statement() {
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
            0
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::AssignStmt),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::DataDecl),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::InvalidStmt),
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
            0
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::InvalidStmt),
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
