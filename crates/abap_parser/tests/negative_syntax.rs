use abap_ast::SyntaxKind;
use abap_parser::{ParseDiagnosticPolicy, ParseResult, parse, parse_with_diagnostic_policy};

fn assert_error_contains(src: &str, needle: &str) {
    let parsed = parse(src);
    assert!(
        parsed.errors.iter().any(|err| err.message.contains(needle)),
        "expected error containing {needle:?} for {src:?}, got {:?}",
        parsed.errors
    );
}

fn assert_parsed_error_contains(parsed: &ParseResult, needle: &str) {
    assert!(
        parsed.errors.iter().any(|err| err.message.contains(needle)),
        "expected error containing {needle:?}, got {:?}",
        parsed.errors
    );
}

fn assert_no_parsed_error_contains(parsed: &ParseResult, needle: &str) {
    assert!(
        parsed
            .errors
            .iter()
            .all(|err| !err.message.contains(needle)),
        "unexpected error containing {needle:?}: {:?}",
        parsed.errors
    );
}

fn assert_kind_count(parsed: &ParseResult, kind: SyntaxKind, expected: usize) {
    assert_eq!(
        parsed.file.count_kind(parsed.file.root(), kind),
        expected,
        "{kind:?}: {:?}",
        parsed.errors
    );
}

fn assert_kind_count_at_least(parsed: &ParseResult, kind: SyntaxKind, expected: usize) {
    let actual = parsed.file.count_kind(parsed.file.root(), kind);
    assert!(
        actual >= expected,
        "{kind:?}: expected at least {expected}, got {actual}: {:?}",
        parsed.errors
    );
}

fn assert_diagnostic_count(parsed: &ParseResult, needle: &str, expected: usize) {
    assert_eq!(
        parsed
            .errors
            .iter()
            .filter(|err| err.message.contains(needle))
            .count(),
        expected,
        "{:?}",
        parsed.errors
    );
}

fn assert_invalid_stmt_count(parsed: &ParseResult, expected: usize) {
    assert_kind_count(parsed, SyntaxKind::InvalidStmt, expected);
}

fn assert_invalid_stmt_count_at_least(parsed: &ParseResult, expected: usize) {
    assert_kind_count_at_least(parsed, SyntaxKind::InvalidStmt, expected);
}

#[test]
fn unmatched_block_boundaries_are_errors_and_recover_to_next_statement() {
    let src = "ENDIF.\nDATA lv TYPE i.\nCATCH cx_root.\nDATA lv_other TYPE i.";
    let parsed = parse(src);

    assert!(
        parsed
            .errors
            .iter()
            .any(|err| err.message.contains("unexpected ENDIF without matching IF")),
        "{:?}",
        parsed.errors
    );
    assert!(
        parsed.errors.iter().any(|err| err
            .message
            .contains("unexpected CATCH without matching TRY")),
        "{:?}",
        parsed.errors
    );
    assert_eq!(
        parsed
            .file
            .count_kind(parsed.file.root(), SyntaxKind::DataDecl),
        2
    );
    assert_kind_count(&parsed, SyntaxKind::Error, 2);
}

#[test]
fn strict_mode_reports_standalone_include_fragment_boundaries() {
    let opening = parse("IF lv_ok = abap_true.\n  lv_value = 1.");
    assert!(
        opening
            .errors
            .iter()
            .any(|err| err.message.contains("expected ENDIF")),
        "{:?}",
        opening.errors
    );

    let closing = parse("ENDIF.");
    assert!(
        closing
            .errors
            .iter()
            .any(|err| err.message.contains("unexpected ENDIF without matching IF")),
        "{:?}",
        closing.errors
    );
}

#[test]
fn include_fragment_policy_suppresses_only_block_boundary_errors() {
    let opening = parse_with_diagnostic_policy(
        "IF lv_ok = abap_true.\n  lv_value = 1.",
        ParseDiagnosticPolicy::IncludeFragment,
    );
    assert!(opening.errors.is_empty(), "{:?}", opening.errors);

    let closing = parse_with_diagnostic_policy("ENDIF.", ParseDiagnosticPolicy::IncludeFragment);
    assert!(closing.errors.is_empty(), "{:?}", closing.errors);

    let malformed = parse_with_diagnostic_policy("IF .", ParseDiagnosticPolicy::IncludeFragment);
    assert!(
        malformed
            .errors
            .iter()
            .any(|err| err.message.contains("expected condition after IF")),
        "{:?}",
        malformed.errors
    );
}

#[test]
fn empty_control_flow_headers_report_specific_errors() {
    let src = concat!(
        "IF . ENDIF.\n",
        "IF ok = abap_true. ELSEIF . ENDIF.\n",
        "WHILE . ENDWHILE.\n",
        "CASE . ENDCASE.\n",
        "CASE lv. WHEN . ENDCASE.\n",
        "TRY. CATCH . ENDTRY."
    );
    let parsed = parse(src);

    for needle in [
        "expected condition after IF",
        "expected condition after ELSEIF",
        "expected condition after WHILE",
        "expected expression after CASE",
        "expected expression after WHEN",
        "expected exception class after CATCH",
    ] {
        assert!(
            parsed.errors.iter().any(|err| err.message.contains(needle)),
            "missing {needle:?}: {:?}",
            parsed.errors
        );
    }
}

#[test]
fn loop_at_requires_source_targets_and_clause_expressions() {
    let src = concat!(
        "LOOP AT . ENDLOOP.\n",
        "LOOP AT itab INTO . ENDLOOP.\n",
        "LOOP AT itab WHERE . ENDLOOP."
    );
    let parsed = parse(src);

    for needle in [
        "expected loop source after LOOP AT",
        "expected target after INTO",
        "expected expression after WHERE",
    ] {
        assert!(
            parsed.errors.iter().any(|err| err.message.contains(needle)),
            "missing {needle:?}: {:?}",
            parsed.errors
        );
    }
    assert_kind_count(&parsed, SyntaxKind::LoopStmt, 0);
    assert_invalid_stmt_count_at_least(&parsed, 3);
}

#[test]
fn assignments_and_inline_data_require_rhs_expressions() {
    let src = "lv_value = .\nDATA(lv_other) = .";
    let parsed = parse(src);

    for needle in [
        "expected assignment value after '='",
        "expected expression after '=' in inline DATA declaration",
    ] {
        assert!(
            parsed.errors.iter().any(|err| err.message.contains(needle)),
            "missing {needle:?}: {:?}",
            parsed.errors
        );
    }
    assert_invalid_stmt_count_at_least(&parsed, 2);
}

#[test]
fn unmatched_delimiters_do_not_hide_following_statement_boundaries() {
    let src = "lv_value = foo ).\nlv_other = foo )\nDATA lv_after TYPE i.";
    let parsed = parse(src);

    assert!(
        parsed
            .errors
            .iter()
            .any(|err| err.message.contains("unmatched closing ')'")),
        "{:?}",
        parsed.errors
    );
    assert_invalid_stmt_count_at_least(&parsed, 2);
    assert_eq!(
        parsed
            .file
            .count_kind(parsed.file.root(), SyntaxKind::DataDecl),
        1
    );
}

#[test]
fn if_and_while_header_unmatched_delimiters_use_result_failure() {
    let src = "IF lv_flag ). ENDIF.\nWHILE lv_flag ]. ENDWHILE.";
    let parsed = parse(src);

    assert_parsed_error_contains(&parsed, "expected '.' after IF condition");
    assert_parsed_error_contains(&parsed, "unexpected ENDIF without matching IF");
    assert_parsed_error_contains(&parsed, "expected '.' after WHILE condition");
    assert_parsed_error_contains(&parsed, "unexpected ENDWHILE without matching WHILE");
    let root = parsed.file.root();
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::IfStmt), 0);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::WhileStmt), 0);
    assert_invalid_stmt_count_at_least(&parsed, 2);
}

#[test]
fn assert_and_check_require_conditions() {
    assert_error_contains("ASSERT .", "expected condition after ASSERT");
    assert_error_contains("CHECK .", "expected condition after CHECK");
}

#[test]
fn simple_statement_missing_period_leaves_next_statement_token() {
    let parsed = parse("ASSERT lv_ok\nDATA lv_after TYPE i.");

    assert_parsed_error_contains(&parsed, "expected '.' to end statement");
    assert_kind_count(&parsed, SyntaxKind::AssertStmt, 0);
    assert_kind_count(&parsed, SyntaxKind::DataDecl, 1);
    assert_invalid_stmt_count_at_least(&parsed, 1);
}

#[test]
fn method_call_missing_period_leaves_next_statement_token() {
    let parsed = parse("lo_prog->add_statement( lo_item )\nDATA lv_after TYPE i.");

    assert_parsed_error_contains(&parsed, "expected '.' after method call");
    assert_kind_count(&parsed, SyntaxKind::CallStmt, 0);
    assert_kind_count(&parsed, SyntaxKind::DataDecl, 1);
    assert_invalid_stmt_count_at_least(&parsed, 1);
}

#[test]
fn result_contract_missing_when_period_leaves_next_statement_token() {
    let parsed = parse(
        "CASE lv_kind. WHEN 'A' WRITE lv_kind. WHEN OTHERS. CLEAR lv_kind. ENDCASE. DATA lv_after TYPE i.",
    );

    assert_diagnostic_count(&parsed, "expected '.'", 1);
    assert_no_parsed_error_contains(&parsed, "unexpected WHEN without matching CASE");
    assert_no_parsed_error_contains(&parsed, "unexpected ENDCASE without matching CASE");
    assert_kind_count(&parsed, SyntaxKind::CaseStmt, 1);
    assert_kind_count(&parsed, SyntaxKind::WhenClause, 1);
    assert_kind_count(&parsed, SyntaxKind::WriteStmt, 1);
    assert_kind_count(&parsed, SyntaxKind::DataDecl, 1);
    assert_invalid_stmt_count_at_least(&parsed, 1);
}

#[test]
fn result_contract_recognized_statement_failure_is_invalid_statement() {
    let parsed = parse("DATA(lv) = . DATA lv_after TYPE i.");

    assert_parsed_error_contains(
        &parsed,
        "expected expression after '=' in inline DATA declaration",
    );
    assert_invalid_stmt_count_at_least(&parsed, 1);
    assert_kind_count(&parsed, SyntaxKind::DataDecl, 1);
}

#[test]
fn result_contract_unknown_significant_token_is_one_invalid_statement() {
    let parsed = parse(") DATA lv_after TYPE i.");

    assert_invalid_stmt_count(&parsed, 1);
    assert_kind_count(&parsed, SyntaxKind::DataDecl, 1);
}

#[test]
fn result_contract_unknown_significant_tokens_progress_one_at_a_time() {
    let parsed = parse(") ] DATA lv_after TYPE i.");

    assert_invalid_stmt_count(&parsed, 2);
    assert_kind_count(&parsed, SyntaxKind::DataDecl, 1);
}

#[test]
fn result_contract_unknown_token_in_if_body_progresses_one_at_a_time() {
    let parsed = parse("IF lv_flag. ) ] DATA lv_inside TYPE i. ENDIF.");

    assert_invalid_stmt_count(&parsed, 2);
    assert_kind_count(&parsed, SyntaxKind::IfStmt, 1);
    assert_kind_count(&parsed, SyntaxKind::DataDecl, 1);
}

#[test]
fn result_contract_unknown_token_in_case_body_progresses_one_at_a_time() {
    let parsed = parse("CASE lv_kind. WHEN 'A'. ) ] DATA lv_inside TYPE i. ENDCASE.");

    assert_invalid_stmt_count(&parsed, 2);
    assert_kind_count(&parsed, SyntaxKind::CaseStmt, 1);
    assert_kind_count(&parsed, SyntaxKind::DataDecl, 1);
}

#[test]
fn result_contract_bad_when_header_does_not_scan_case_body() {
    let parsed =
        parse("CASE lv_kind. WHEN = 1. DATA lv_inside TYPE i. ENDCASE. DATA lv_after TYPE i.");

    assert_parsed_error_contains(&parsed, "expected expression after WHEN");
    assert_no_parsed_error_contains(&parsed, "unexpected ENDCASE without matching CASE");
    assert_kind_count(&parsed, SyntaxKind::CaseStmt, 1);
    assert_kind_count(&parsed, SyntaxKind::WhenClause, 0);
    assert_kind_count(&parsed, SyntaxKind::DataDecl, 2);
    assert_invalid_stmt_count_at_least(&parsed, 1);
}

#[test]
fn case_when_missing_period_uses_blunt_invalid_statement() {
    let src = concat!(
        "CASE lv_kind.\n",
        "  WHEN 'A'\n",
        "    lv_a = 1.\n",
        "  WHEN 'B'.\n",
        "    lv_b = 2.\n",
        "ENDCASE.\n",
        "DATA lv_after TYPE i."
    );
    let parsed = parse(src);
    let period_error = parsed
        .errors
        .iter()
        .find(|err| err.message.contains("expected '.'"))
        .expect("missing expected-period diagnostic");

    assert_parsed_error_contains(&parsed, "expected '.'");
    assert_no_parsed_error_contains(&parsed, "unexpected WHEN without matching CASE");
    assert_no_parsed_error_contains(&parsed, "unexpected ENDCASE without matching CASE");
    assert_eq!(
        period_error.range,
        src.find("\n    lv_a").unwrap()..src.find("lv_a = 1").unwrap()
    );
    assert_kind_count(&parsed, SyntaxKind::CaseStmt, 1);
    assert_kind_count(&parsed, SyntaxKind::WhenClause, 1);
    assert_kind_count(&parsed, SyntaxKind::AssignStmt, 2);
    assert_kind_count(&parsed, SyntaxKind::DataDecl, 1);
    assert_invalid_stmt_count_at_least(&parsed, 1);
}

#[test]
fn case_when_bad_expression_uses_blunt_invalid_statement() {
    let src = concat!(
        "CASE lv_kind.\n",
        "  WHEN = 1.\n",
        "    lv_bad = 1.\n",
        "  WHEN 'B'.\n",
        "    lv_good = 2.\n",
        "ENDCASE.\n",
        "DATA lv_after TYPE i."
    );
    let parsed = parse(src);

    assert_parsed_error_contains(&parsed, "expected expression after WHEN");
    assert_no_parsed_error_contains(&parsed, "unexpected WHEN without matching CASE");
    assert_no_parsed_error_contains(&parsed, "unexpected ENDCASE without matching CASE");
    assert_kind_count(&parsed, SyntaxKind::CaseStmt, 1);
    assert_kind_count(&parsed, SyntaxKind::WhenClause, 1);
    assert_kind_count(&parsed, SyntaxKind::AssignStmt, 2);
    assert_kind_count(&parsed, SyntaxKind::DataDecl, 1);
    assert_invalid_stmt_count_at_least(&parsed, 1);
}

#[test]
fn case_when_rejects_non_operand_forms_validated_in_sap() {
    for src in [
        "CASE lv_num. WHEN 1 + 1. lv = 1. ENDCASE.",
        "CASE lv_num. WHEN lv_num = 1. lv = 1. ENDCASE.",
        "CASE lv_num. WHEN lv_num > 1. lv = 1. ENDCASE.",
        "CASE lv_num. WHEN 1 TO 5. lv = 1. ENDCASE.",
        "CASE lv_num. WHEN lt_int[ 1 ]. lv = 1. ENDCASE.",
    ] {
        let parsed = parse(src);
        assert!(
            parsed.errors.iter().any(|err| {
                err.message.contains("invalid operand after WHEN")
                    || err.message.contains("expected expression after WHEN")
            }),
            "{src}: {:?}",
            parsed.errors
        );
        assert_no_parsed_error_contains(&parsed, "unexpected ENDCASE without matching CASE");
        assert_kind_count(&parsed, SyntaxKind::CaseStmt, 1);
        assert_invalid_stmt_count_at_least(&parsed, 1);
    }
}

#[test]
fn elseif_missing_period_uses_blunt_invalid_statement() {
    let src = concat!(
        "IF lv_a = 1.\n",
        "  lv_a = 2.\n",
        "ELSEIF lv_b = 2\n",
        "  lv_b = 3.\n",
        "ELSE.\n",
        "  lv_c = 4.\n",
        "ENDIF.\n",
        "DATA lv_after TYPE i."
    );
    let parsed = parse(src);

    assert_parsed_error_contains(&parsed, "expected '.' after ELSEIF condition");
    assert_kind_count(&parsed, SyntaxKind::IfStmt, 0);
    assert_kind_count(&parsed, SyntaxKind::ElseifClause, 0);
    assert_kind_count(&parsed, SyntaxKind::ElseClause, 0);
    assert_kind_count(&parsed, SyntaxKind::AssignStmt, 2);
    assert_kind_count(&parsed, SyntaxKind::DataDecl, 1);
    assert_invalid_stmt_count_at_least(&parsed, 1);
}

#[test]
fn missing_rhs_recovery_preserves_following_statement() {
    let assignment = parse("lv_bad = .\nlv_after = 1.");
    assert_parsed_error_contains(&assignment, "expected assignment value after '='");
    assert_kind_count(&assignment, SyntaxKind::AssignStmt, 1);
    assert_kind_count(&assignment, SyntaxKind::Error, 0);
    assert_invalid_stmt_count_at_least(&assignment, 1);

    let inline = parse("DATA(lv_bad) = .\nDATA lv_after TYPE i.");
    assert_parsed_error_contains(
        &inline,
        "expected expression after '=' in inline DATA declaration",
    );
    assert_kind_count(&inline, SyntaxKind::DataDecl, 1);
    assert_kind_count(&inline, SyntaxKind::Error, 0);
    assert_invalid_stmt_count_at_least(&inline, 1);
}
