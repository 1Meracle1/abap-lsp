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
fn classifies_classic_arithmetic_string_and_list_statements() {
    let src = concat!(
        "ADD 1 TO lv_cnt.\n",
        "ADD lv_object_cnt TO lv_objects_deleted.\n",
        "SUBTRACT 1 FROM lv_cnt.\n",
        "SUBTRACT lv_used FROM lv_total.\n",
        "TRANSLATE lv_text TO UPPER CASE.\n",
        "TRANSLATE lv_text USING ' _'.\n",
        "shift l_strind left deleting leading space.\n",
        "SHIFT lv_text RIGHT BY 1 PLACES.\n",
        "SEARCH lt_lines FOR lv_pattern.\n",
        "SEARCH lv_text FOR 'ABC' STARTING AT lv_offset.\n",
        "FORMAT COLOR COL_HEADING INTENSIFIED.\n",
        "FORMAT RESET.\n",
        "POSITION e-lopind.\n",
        "HIDE mara-matnr.\n",
        "SUPPRESS DIALOG.\n"
    );
    let parsed = parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let root = parsed.file.root();
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::AddStmt), 2);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::SubtractStmt), 2);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::TranslateStmt), 2);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::ShiftStmt), 2);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::SearchStmt), 2);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::FormatStmt), 2);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::PositionStmt), 1);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::HideStmt), 1);
    assert_eq!(
        parsed.file.count_kind(root, SyntaxKind::SuppressDialogStmt),
        1
    );
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
}

#[test]
fn classifies_classic_list_dynpro_and_extract_dataset_corpus_statements() {
    let src = concat!(
        "READ LINE BAN-SZEIL OF PAGE BAN-PAGE INDEX LSIND.\n",
        "MODIFY LINE sy-index\n",
        "            FIELD FORMAT info COLOR = color_positive.\n",
        "AUTHORITY-CHECK OBJECT lc_auth_obj ID lc_bukrs\n",
        "  FIELD lw_mat_info-bukrs ID lc_actvt FIELD lc_display.\n",
        "FIELD-GROUPS: HEADER, KOPF, POS.\n",
        "INSERT DUMMY\n",
        "       EKKO-EKORG EKKO-LIFNR EKKO-EKGRP.\n",
        "FIELD screen_field MODULE check_input.\n",
    );
    let parsed = parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let root = parsed.file.root();
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::ReadLineStmt), 1);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::ModifyLineStmt), 1);
    assert_eq!(
        parsed.file.count_kind(root, SyntaxKind::AuthorityCheckStmt),
        1
    );
    assert_eq!(
        parsed
            .file
            .count_kind(root, SyntaxKind::AuthorityCheckIdClause),
        2
    );
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::FieldGroupsStmt), 1);
    assert_eq!(
        parsed.file.count_kind(root, SyntaxKind::InsertExtractStmt),
        1
    );
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::FieldStmt), 1);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
}

#[test]
fn classifies_runtime_generated_and_dynpro_statements() {
    let src = concat!(
        "CALL TRANSACTION u_tcode WITH AUTHORITY-CHECK      \"#EC CI_CALLTA\n",
        "                         AND SKIP FIRST SCREEN.\n",
        "OPEN DATASET ds_phy_name IN TEXT MODE FOR INPUT\n",
        "             ENCODING DEFAULT.\n",
        "READ DATASET ds_phy_name INTO wa.\n",
        "TRANSFER wa TO ds_phy_name.\n",
        "CLOSE DATASET ds_phy_name.\n",
        "DELETE DATASET ds_phy_name.\n",
        "READ TEXTPOOL MASTER_FPOOL INTO TEXTPOOL_TAB LANGUAGE SY-LANGU.\n",
        "INSERT TEXTPOOL lv_progname FROM lt_textpool.\n",
        "GENERATE SUBROUTINE POOL lt_source_code NAME l_program_pool \"#EC CI_GENERATE\n",
        "  MESSAGE l_message LINE l_line WORD l_word OFFSET l_offset.\n",
        "GENERATE DYNPRO h f e m ID i_dynid.\n",
        "SET SCREEN sy-dynnr.\n",
        "SET CURSOR FIELD l_dynpro_field-screenname.\n",
        "SET CURSOR 2 LS-CLINE.\n",
        "GET CURSOR FIELD l_field.\n",
        "GET CURSOR FIELD f LINE l OFFSET o.\n",
        "GET BADI lr_runtime.\n",
        "SET HANDLER lcl_alv_handler=>added_function FOR events.\n"
    );
    let parsed = parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let root = parsed.file.root();
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::CallStmt), 1);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::OpenDatasetStmt), 1);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::ReadDatasetStmt), 1);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::TransferStmt), 1);
    assert_eq!(
        parsed.file.count_kind(root, SyntaxKind::CloseDatasetStmt),
        1
    );
    assert_eq!(
        parsed.file.count_kind(root, SyntaxKind::DeleteDatasetStmt),
        1
    );
    assert_eq!(
        parsed.file.count_kind(root, SyntaxKind::ReadTextpoolStmt),
        1
    );
    assert_eq!(
        parsed.file.count_kind(root, SyntaxKind::InsertTextpoolStmt),
        1
    );
    assert_eq!(
        parsed
            .file
            .count_kind(root, SyntaxKind::GenerateSubroutinePoolStmt),
        1
    );
    assert_eq!(
        parsed.file.count_kind(root, SyntaxKind::GenerateDynproStmt),
        1
    );
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::SetScreenStmt), 1);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::SetCursorStmt), 2);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::GetCursorStmt), 2);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::GetBadiStmt), 1);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::SetHandlerStmt), 1);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::UnparsedStmt), 0);
    assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
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
