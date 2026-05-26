use abap_ast::SyntaxKind;
use abap_parser::parse;

fn assert_kind_count(src: &str, kind: SyntaxKind, expected: usize) {
    let parsed = parse(src);
    assert!(parsed.errors.is_empty(), "{src:?} => {:?}", parsed.errors);
    assert_eq!(
        parsed.file.count_kind(parsed.file.root(), kind),
        expected,
        "{src:?}"
    );
}

#[test]
fn ports_legacy_data_and_statics_shapes() {
    assert_kind_count("DATA(lv_value) = 1.", SyntaxKind::DataInlineDecl, 1);
    assert_kind_count("DATA lv_value TYPE i.", SyntaxKind::DataDecl, 1);
    assert_kind_count("DATA: lv_a, lv_b TYPE i.", SyntaxKind::DataDecl, 1);
    assert_kind_count("CLASS-DATA gv_value TYPE i.", SyntaxKind::DataDecl, 1);
    assert_kind_count(
        "STATICS sv_last_tzone TYPE tznzone.",
        SyntaxKind::StaticsDecl,
        1,
    );
    assert_kind_count(
        "STATICS: sv_flag, sv_mode TYPE c.",
        SyntaxKind::StaticsDecl,
        1,
    );
    assert_kind_count("TABLES: tbtco, v_op.", SyntaxKind::TablesDecl, 1);
    assert_kind_count("TABLES: *t001, t005.", SyntaxKind::TablesDecl, 1);
}

#[test]
fn ports_legacy_types_constants_and_field_symbols_shapes() {
    assert_kind_count("TYPES ty_counter TYPE i.", SyntaxKind::TypesDecl, 1);
    assert_kind_count(
        "CONSTANTS lcv_max(14) TYPE p DECIMALS 7 VALUE '0.9999999'.",
        SyntaxKind::ConstantsDecl,
        1,
    );
    assert_kind_count(
        "FIELD-SYMBOLS <line> LIKE LINE OF itab.",
        SyntaxKind::FieldSymbolsDecl,
        1,
    );
}

#[test]
fn ports_generated_include_declaration_shapes() {
    assert_kind_count(
        "DATA: BEGIN OF itab OCCURS 10, field, INCLUDE STRUCTURE textpool, END OF itab.",
        SyntaxKind::DataDecl,
        1,
    );
    assert_kind_count(
        "CONSTANTS: c_action(4) VALUE 'SAVE', c_mode TYPE c VALUE 'X'.",
        SyntaxKind::ConstantsDecl,
        1,
    );
    assert_kind_count(
        "CONSTANTS: back(4) TYPE c VALUE 'BACK', end(4) TYPE c VALUE 'ENDE'.",
        SyntaxKind::ConstantsDecl,
        1,
    );
    assert_kind_count(
        "FIELD-SYMBOLS: <fs_any>, <fs_line> LIKE LINE OF lt_tab.",
        SyntaxKind::FieldSymbolsDecl,
        1,
    );
    assert_kind_count(
        "RANGES: mark_functions FOR sy-ucomm, obj_range FOR objh-objectname.",
        SyntaxKind::RangesDecl,
        1,
    );
    assert_kind_count(
        "CONTROLS: tc TYPE TABLEVIEW USING SCREEN 0100, ts TYPE TABSTRIP.",
        SyntaxKind::ControlsDecl,
        1,
    );
}

#[test]
fn ports_legacy_load_and_pool_statements() {
    assert_kind_count("TYPE-POOLS: cxtab, vimty.", SyntaxKind::TypePoolsStmt, 1);
    assert_kind_count(
        "FUNCTION-POOL zfg MESSAGE-ID sv.",
        SyntaxKind::FunctionPoolStmt,
        1,
    );
    assert_kind_count(
        "CLASS cl_gui_cfw DEFINITION LOAD.",
        SyntaxKind::ClassLoadStmt,
        1,
    );
}
