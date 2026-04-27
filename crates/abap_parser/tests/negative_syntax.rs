use abap_ast::SyntaxKind;
use abap_parser::{ParseDiagnosticPolicy, parse, parse_with_diagnostic_policy};

fn assert_error_contains(src: &str, needle: &str) {
    let parsed = parse(src);
    assert!(
        parsed.errors.iter().any(|err| err.message.contains(needle)),
        "expected error containing {needle:?} for {src:?}, got {:?}",
        parsed.errors
    );
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
    assert!(
        parsed
            .file
            .count_kind(parsed.file.root(), SyntaxKind::Error)
            >= 2
    );
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
        "CASE . WHEN . ENDCASE.\n",
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
    assert_eq!(
        parsed
            .file
            .count_kind(parsed.file.root(), SyntaxKind::LoopStmt),
        3
    );
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
    assert!(
        parsed
            .file
            .count_kind(parsed.file.root(), SyntaxKind::Error)
            >= 2
    );
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
    assert!(
        parsed.errors.iter().any(|err| err
            .message
            .contains("expected '.' to end assignment statement")),
        "{:?}",
        parsed.errors
    );
    assert_eq!(
        parsed
            .file
            .count_kind(parsed.file.root(), SyntaxKind::DataDecl),
        1
    );
}

#[test]
fn control_flow_headers_report_unmatched_delimiters() {
    let src =
        "IF lv_flag ). ENDIF.\nWHILE lv_flag ]. ENDWHILE.\nCASE lv_kind }. WHEN OTHERS. ENDCASE.";
    let parsed = parse(src);

    for needle in [
        "unmatched closing ')'",
        "unmatched closing ']'",
        "unmatched closing '}'",
    ] {
        assert!(
            parsed.errors.iter().any(|err| err.message.contains(needle)),
            "missing {needle:?}: {:?}",
            parsed.errors
        );
    }
    let root = parsed.file.root();
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::IfStmt), 1);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::WhileStmt), 1);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::CaseStmt), 1);
}

#[test]
fn assert_and_check_require_conditions() {
    assert_error_contains("ASSERT .", "expected condition after ASSERT");
    assert_error_contains("CHECK .", "expected condition after CHECK");
}
