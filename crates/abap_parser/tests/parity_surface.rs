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
    let src = "FIND FIRST OCCURRENCE OF | | IN iv_tag_path MATCH OFFSET lv_first_sep.";
    let parsed = parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let root = parsed.file.root();
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::FindStmt), 1);
}

#[test]
fn program_statement_is_classified_as_report_stmt() {
    let src = "PROGRAM rsnast00 MESSAGE-ID vn.\n";
    let parsed = parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let root = parsed.file.root();
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::ReportStmt), 1);
}

#[test]
fn parses_amdp_database_function_body_as_sqlscript_island() {
    let src = r#"
CLASS zcl_adt DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_amdp_marker_hdb.
    CLASS-METHODS get_origin FOR TABLE FUNCTION zmanf_cntry.
ENDCLASS.

CLASS zcl_adt IMPLEMENTATION.
  METHOD get_origin BY DATABASE FUNCTION FOR HDB LANGUAGE SQLSCRIPT OPTIONS READ-ONLY
                    USING zattp_t_mat_prp.
    lt_origin = APPLY_FILTER ( zattp_t_mat_prp, :lt_evt_rel );

    RETURN SELECT zattp_t_mat_prp.mandt,
                  zattp_t_mat_prp.matnr
             FROM :lt_origin zattp_t_mat_prp;
  ENDMETHOD.
ENDCLASS.
"#;

    let parsed = parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let root = parsed.file.root();
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::MethodDecl), 1);
    assert_eq!(
        parsed.file.count_kind(root, SyntaxKind::MethodDeclTarget),
        1
    );
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::SqlScriptIsland), 1);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
}

#[test]
fn parses_amdp_database_procedure_body_as_sqlscript_island() {
    let src = r#"
CLASS zcl_attp_ua_dep_rl_amdp DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_amdp_marker_hdb.
    CLASS-METHODS get_rel_rep_evt.
ENDCLASS.

CLASS zcl_attp_ua_dep_rl_amdp IMPLEMENTATION.
  METHOD get_rel_rep_evt
         BY DATABASE PROCEDURE
         FOR HDB
         LANGUAGE SQLSCRIPT
         OPTIONS READ-ONLY
         USING /sttp/rep_evt.
    et_rep_dep = SELECT rep_evtid, evtid, rule_type, status_response
                   FROM "/STTP/REP_EVT" AS rep
                   INNER JOIN :it_ua_dp_rl AS ua
                   ON rep.rule_type = ua.previousruletype;

    et_rep_trig = SELECT DISTINCT rep.rep_evtid, rep.evtid
                    FROM "/STTP/REP_EVT" AS rep
                    INNER JOIN :et_rep_dep AS res
                    ON rep.evtid = res.evtid;
  ENDMETHOD.
ENDCLASS.
"#;

    let parsed = parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let root = parsed.file.root();
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::MethodDecl), 1);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::SqlScriptIsland), 1);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::SelectStmt), 0);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
}
