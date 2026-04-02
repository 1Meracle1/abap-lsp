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
    assert_kind_count(
        "STATICS sv_last_tzone TYPE tznzone.",
        SyntaxKind::StaticsDecl,
        1,
    );
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
