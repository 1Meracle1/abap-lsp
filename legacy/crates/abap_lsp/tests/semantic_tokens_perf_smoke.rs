use std::fmt::Write as _;
use std::str::FromStr;
use std::time::Instant;

use abap_lsp::{ServerState, publish_open_document, semantic_tokens};
use lsp_types::{
    DidOpenTextDocumentParams, SemanticTokensParams, TextDocumentIdentifier, TextDocumentItem, Uri,
};

fn semantic_tokens_perf_source(repetitions: usize) -> String {
    let mut text = String::from(
        "CLASS zcl_perf DEFINITION.\n\
           PUBLIC SECTION.\n\
             TYPES: BEGIN OF ty_row,\n\
                      value TYPE i,\n\
                    END OF ty_row.\n\
             METHODS run.\n\
             METHODS consume IMPORTING iv_value TYPE i.\n\
         ENDCLASS.\n\
         \n\
         CLASS zcl_perf IMPLEMENTATION.\n\
           METHOD run.\n",
    );
    for idx in 0..repetitions {
        writeln!(text, "    DATA ls_row_{idx} TYPE ty_row.").expect("write data line");
        writeln!(text, "    ls_row_{idx}-value = {idx}.").expect("write field access line");
        writeln!(text, "    consume( iv_value = ls_row_{idx}-value ).")
            .expect("write named argument line");
    }
    text.push_str(
        "  ENDMETHOD.\n\
           METHOD consume.\n\
           ENDMETHOD.\n\
         ENDCLASS.\n",
    );
    text
}

#[test]
#[ignore = "manual semantic token request throughput smoke"]
fn semantic_tokens_full_request_throughput_smoke() {
    let state = ServerState::default();
    let text = semantic_tokens_perf_source(250);
    let uri = Uri::from_str("file:///semantic_tokens_perf.abap").expect("uri");
    publish_open_document(
        &state,
        &DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri.clone(),
                language_id: "abap".to_string(),
                version: 1,
                text: text.clone(),
            },
        },
    );

    let params = SemanticTokensParams {
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
        text_document: TextDocumentIdentifier { uri },
    };

    let warmup = semantic_tokens(&state, &params).expect("semantic tokens");
    assert!(
        !warmup.data.is_empty(),
        "expected semantic tokens from generated perf sample"
    );

    let iterations = 20;
    let start = Instant::now();
    let mut total_tokens = 0usize;
    for _ in 0..iterations {
        total_tokens += semantic_tokens(&state, &params)
            .expect("semantic tokens")
            .data
            .len();
    }
    let elapsed = start.elapsed();

    eprintln!(
        concat!(
            "semantic tokens request perf sample\n",
            "bytes={} lines={} iterations={}\n",
            "tokens_per_request={} total_tokens={}\n",
            "elapsed={:?}\n"
        ),
        text.len(),
        text.lines().count(),
        iterations,
        warmup.data.len(),
        total_tokens,
        elapsed
    );
}
