use abap_ast::SyntaxKind;
use abap_parser::parse;

#[test]
fn ports_core_control_flow_shapes() {
    let src = concat!(
        "WHILE lv_count > 0. lv_count = lv_count - 1. ENDWHILE.\n",
        "DO 2 TIMES. CONTINUE. ENDDO.\n",
        "DO lv_n TIMES. DO 3 TIMES. EXIT. ENDDO. ENDDO.\n",
        "START-OF-SELECTION. STOP. END-OF-SELECTION. WRITE 'done'.\n",
        "CASE lv_kind. WHEN 'A'. WRITE 'a'. WHEN OTHERS. WRITE 'b'. ENDCASE.\n",
        "TRY. LOOP AT itab INTO wa. EXIT. ENDLOOP. CATCH cx_root. WRITE 'x'. ENDTRY."
    );
    let parsed = parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let root = parsed.file.root();
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::WhileStmt), 1);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::DoStmt), 3);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::CaseStmt), 1);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::WhenClause), 2);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::TryStmt), 1);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::LoopStmt), 1);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::StopStmt), 1);
}
