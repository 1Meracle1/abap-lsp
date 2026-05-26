use abap_parser::parse;
use abap_symbols::{
    Diagnostic, DiagnosticKind, UnitId, analyze_project_from_units, analyze_unit,
    build_project_routine_analysis,
};

fn routine_diagnostics(src: &str) -> Vec<Diagnostic> {
    let parsed = parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let unit = analyze_unit("file:///read_table_binary_search.abap", src, &parsed);
    let project = analyze_project_from_units(vec![unit]);
    let routine_analysis = build_project_routine_analysis(&project);
    routine_analysis.diagnostics_for_unit(UnitId(0)).to_vec()
}

fn binary_search_order_warnings(src: &str) -> Vec<Diagnostic> {
    routine_diagnostics(src)
        .into_iter()
        .filter(|diag| diag.kind == DiagnosticKind::UnsortedReadTableBinarySearch)
        .collect()
}

fn fixture(body: &str) -> String {
    format!(
        r#"
FORM run.
  TYPES: BEGIN OF ty_row,
           carrid TYPE string,
           connid TYPE string,
         END OF ty_row.
  DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
  DATA ls_row TYPE ty_row.

{body}
ENDFORM.
"#
    )
}

#[test]
fn warns_on_binary_search_without_prior_sort_or_order_by() {
    let src = fixture(
        "  READ TABLE lt_rows INTO ls_row WITH KEY carrid = 'AA' connid = '001' BINARY SEARCH.",
    );

    let warnings = binary_search_order_warnings(&src);
    assert_eq!(warnings.len(), 1, "{warnings:#?}");
    assert_eq!(&src[warnings[0].range.clone()], "BINARY SEARCH");
    assert!(warnings[0].message.contains("carrid, connid"));
}

#[test]
fn accepts_binary_search_after_sort_by_same_key_fields() {
    let src = fixture(
        r#"  SORT lt_rows BY carrid connid.
  READ TABLE lt_rows INTO ls_row WITH KEY carrid = 'AA' connid = '001' BINARY SEARCH."#,
    );

    let warnings = binary_search_order_warnings(&src);
    assert!(warnings.is_empty(), "{warnings:#?}");
}

#[test]
fn warns_when_prior_sort_uses_different_field_order() {
    let src = fixture(
        r#"  SORT lt_rows BY connid carrid.
  READ TABLE lt_rows INTO ls_row WITH KEY carrid = 'AA' connid = '001' BINARY SEARCH."#,
    );

    let warnings = binary_search_order_warnings(&src);
    assert_eq!(warnings.len(), 1, "{warnings:#?}");
}

#[test]
fn accepts_binary_search_after_select_into_table_order_by_same_fields() {
    let src = fixture(
        r#"  SELECT carrid, connid
    FROM zflights
    INTO TABLE @lt_rows
    ORDER BY carrid, connid.
  READ TABLE lt_rows INTO ls_row WITH KEY carrid = 'AA' connid = '001' BINARY SEARCH."#,
    );

    let warnings = binary_search_order_warnings(&src);
    assert!(warnings.is_empty(), "{warnings:#?}");
}

#[test]
fn accepts_binary_search_after_select_into_table_order_by_primary_key() {
    let src = r#"
TYPES: BEGIN OF zflights,
         carrid TYPE string, " key; carrier
         connid TYPE string, " key; connection
       END OF zflights.

FORM run.
  TYPES ty_row TYPE zflights.
  DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
  DATA ls_row TYPE ty_row.

  SELECT carrid, connid
    FROM zflights
    INTO TABLE @lt_rows
    ORDER BY PRIMARY KEY.
  READ TABLE lt_rows INTO ls_row WITH KEY carrid = 'AA' connid = '001' BINARY SEARCH.
ENDFORM.
"#;

    let warnings = binary_search_order_warnings(src);
    assert!(warnings.is_empty(), "{warnings:#?}");
}
