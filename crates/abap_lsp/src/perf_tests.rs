use std::env;
use std::fs;
use std::path::PathBuf;
use std::str::FromStr;
use std::time::Instant;

use lsp_types::{DidOpenTextDocumentParams, TextDocumentItem, Uri};

use crate::{ServerState, publish_open_document, sem_tokens};

const DEFAULT_SAMPLE_PATH: &str =
    r"D:\dev\abap\lsp_development_examples2\.abapls\cache\sources\%2FSTTP%2FCL_MD_BPARTNER.abap";
const PERF_SAMPLE_URI: &str = "file:///semantic_tokens_perf_sample.abap";

fn perf_sample_path() -> PathBuf {
    env::var("ABAP_PERF_SAMPLE")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from(DEFAULT_SAMPLE_PATH))
}

fn perf_iterations() -> usize {
    env::var("ABAP_PERF_ITERATIONS")
        .ok()
        .and_then(|raw| raw.parse::<usize>().ok())
        .filter(|value| *value > 0)
        .unwrap_or(10)
}

fn load_perf_sample() -> (PathBuf, String) {
    let path = perf_sample_path();
    let source = fs::read_to_string(&path)
        .unwrap_or_else(|err| panic!("failed to read perf sample '{}': {err}", path.display()));
    (path, source)
}

#[test]
#[ignore = "manual large-file semantic token build perf check"]
fn large_file_build_semantic_tokens_breakdown() {
    let (path, text) = load_perf_sample();
    let line_count = text.lines().count();
    assert!(
        line_count >= 10_000,
        "expected a large ABAP sample, got only {line_count} lines from '{}'",
        path.display()
    );

    let state = ServerState::default();
    let uri = Uri::from_str(PERF_SAMPLE_URI).expect("uri");

    let publish_start = Instant::now();
    let snapshot = publish_open_document(
        &state,
        &DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri,
                language_id: "abap".to_string(),
                version: 1,
                text: text.clone(),
            },
        },
    );
    let publish_elapsed = publish_start.elapsed();

    let warmup_start = Instant::now();
    let warmup = sem_tokens::build_semantic_tokens(snapshot.as_ref());
    let warmup_elapsed = warmup_start.elapsed();
    assert!(
        !warmup.data.is_empty(),
        "expected semantic tokens from large perf sample '{}'",
        path.display()
    );

    let iterations = perf_iterations();
    let build_start = Instant::now();
    let mut total_tokens = 0usize;
    for _ in 0..iterations {
        total_tokens += sem_tokens::build_semantic_tokens(snapshot.as_ref())
            .data
            .len();
    }
    let build_elapsed = build_start.elapsed();

    eprintln!(
        concat!(
            "semantic tokens build perf sample: {}\n",
            "bytes={} lines={} parse_errors={}\n",
            "symbols={} references={} field_accesses={} named_arguments={}\n",
            "publish_and_analyze={:?}\n",
            "build_warmup={:?}\n",
            "build_iterations={} tokens_per_build={} total_tokens={}\n",
            "build_total={:?}\n"
        ),
        path.display(),
        text.len(),
        line_count,
        snapshot.parse.errors.len(),
        snapshot.symbols.symbols.len(),
        snapshot.symbols.references.len(),
        snapshot.symbols.field_accesses.len(),
        snapshot.symbols.named_arguments.len(),
        publish_elapsed,
        warmup_elapsed,
        iterations,
        warmup.data.len(),
        total_tokens,
        build_elapsed,
    );
}
