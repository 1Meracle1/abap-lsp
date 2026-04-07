use abap_ast::SyntaxKind;
use abap_parser::parse;

#[test]
fn ports_program_oop_and_sql_surface_shapes() {
    let src = concat!(
        "REPORT zfoo.\n",
        "INCLUDE zinc_foo.\n",
        "START-OF-SELECTION.\n",
        "FORM run.\n",
        "  MODULE status_0100 OUTPUT.\n",
        "    CLASS lcl IMPLEMENTATION.\n",
        "      METHOD execute.\n",
        "        SELECT * FROM t INTO wa.\n",
        "          READ TABLE itab INTO wa INDEX 1.\n",
        "          WRITE wa.\n",
        "        ENDSELECT.\n",
        "      ENDMETHOD.\n",
        "    ENDCLASS.\n",
        "  ENDMODULE.\n",
        "ENDFORM.\n"
    );
    let parsed = parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let root = parsed.file.root();
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::ReportStmt), 1);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::IncludeStmt), 1);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::EventBlock), 1);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::FormDecl), 1);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::ModuleDecl), 1);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::ClassDecl), 1);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::MethodDecl), 1);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::SelectStmt), 1);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::ReadTableStmt), 1);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::WriteStmt), 1);
}

#[test]
fn classifies_find_match_offset_statement() {
    let src =
        "FIND FIRST OCCURRENCE OF | | IN iv_tag_path MATCH OFFSET lv_first_sep.";
    let parsed = parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let root = parsed.file.root();
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::FindStmt), 1);
}
