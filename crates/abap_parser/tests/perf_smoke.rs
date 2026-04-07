use std::time::Instant;

use abap_parser::parse;

fn perf_iterations() -> usize {
    std::env::var("ABAP_PARSER_PERF_ITERATIONS")
        .ok()
        .and_then(|raw| raw.parse::<usize>().ok())
        .filter(|value| *value > 0)
        .unwrap_or(2_000)
}

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

    let iterations = perf_iterations();
    let start = Instant::now();
    for _ in 0..iterations {
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    }
    let elapsed = start.elapsed();
    eprintln!(
        "parsed throughput smoke: bytes={} iterations={} elapsed={:?}",
        src.len(),
        iterations,
        elapsed
    );
}
