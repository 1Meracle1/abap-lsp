use std::time::Instant;

use abap_parser::parse;

#[test]
#[ignore = "manual throughput smoke check"]
fn parser_throughput_smoke() {
    let src = concat!(
        "REPORT zperf.\n",
        "DATA(lv_obj) = NEW zcl_demo( ).\n",
        "TYPES: ty_id TYPE i, ty_name TYPE string.\n",
        "CONSTANTS lc_flag TYPE c VALUE 'X'.\n",
        "FIELD-SYMBOLS <line> LIKE LINE OF itab.\n",
        "START-OF-SELECTION.\n",
        "FORM run.\n",
        "  WHILE lv_count > 0. lv_count = lv_count - 1. ENDWHILE.\n",
        "  CASE lv_kind. WHEN 'A'. WRITE 'a'. WHEN OTHERS. WRITE 'b'. ENDCASE.\n",
        "  TRY. SELECT * FROM t INTO wa. READ TABLE itab INTO wa INDEX 1. ENDSELECT. CATCH cx_root. ENDTRY.\n",
        "ENDFORM.\n"
    );

    let start = Instant::now();
    for _ in 0..2_000 {
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    }
    eprintln!("parsed throughput smoke in {:?}", start.elapsed());
}
