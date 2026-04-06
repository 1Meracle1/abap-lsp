use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::str::FromStr;
use std::time::{Duration, Instant};

use abap_lsp::{ServerState, build_semantic_tokens, publish_open_document};
use lsp_types::{DidOpenTextDocumentParams, TextDocumentItem, Uri};

const DEFAULT_SAMPLE_PATH: &str =
    r"D:\dev\abap\lsp_development_examples2\.abapls\cache\sources\%2FSTTP%2FCL_MD_BPARTNER.abap";
const PERF_SAMPLE_URI: &str = "file:///semantic_tokens_perf_sample.abap";

struct Config {
    sample_path: PathBuf,
    iterations: usize,
    warmup_iterations: usize,
}

fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(message) => {
            eprintln!("{message}");
            ExitCode::FAILURE
        }
    }
}

fn run() -> Result<(), String> {
    let config = parse_args(env::args().skip(1))?;
    let text = fs::read_to_string(&config.sample_path).map_err(|err| {
        format!(
            "failed to read perf sample '{}': {err}",
            config.sample_path.display()
        )
    })?;
    let line_count = text.lines().count();
    if line_count < 10_000 {
        return Err(format!(
            "expected a large ABAP sample, got only {line_count} lines from '{}'",
            config.sample_path.display()
        ));
    }

    let state = ServerState::default();
    let uri = Uri::from_str(PERF_SAMPLE_URI).map_err(|err| format!("invalid perf uri: {err}"))?;

    let publish_start = Instant::now();
    let snapshot = publish_open_document(
        &state,
        &DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri,
                language_id: "abap".to_string(),
                version: 1,
                text,
            },
        },
    );
    let publish_elapsed = publish_start.elapsed();

    let mut warmup_tokens = 0usize;
    let warmup_start = Instant::now();
    for _ in 0..config.warmup_iterations {
        warmup_tokens = build_semantic_tokens(snapshot.as_ref()).data.len();
    }
    let warmup_elapsed = warmup_start.elapsed();
    if warmup_tokens == 0 {
        return Err(format!(
            "expected semantic tokens from large perf sample '{}'",
            config.sample_path.display()
        ));
    }

    let measure_start = Instant::now();
    let mut total_tokens = 0usize;
    for _ in 0..config.iterations {
        total_tokens += build_semantic_tokens(snapshot.as_ref()).data.len();
    }
    let measure_elapsed = measure_start.elapsed();

    println!(
        "semantic tokens build perf sample: {}",
        config.sample_path.display()
    );
    println!(
        "bytes={} lines={} parse_errors={}",
        snapshot.text.len(),
        line_count,
        snapshot.parse.errors.len()
    );
    println!(
        "symbols={} references={} field_accesses={} named_arguments={}",
        snapshot.symbols.symbols.len(),
        snapshot.symbols.references.len(),
        snapshot.symbols.field_accesses.len(),
        snapshot.symbols.named_arguments.len()
    );
    println!("publish_and_analyze={publish_elapsed:?}");
    println!(
        "warmup_iterations={} warmup_total={warmup_elapsed:?} warmup_per_iter={:?}",
        config.warmup_iterations,
        duration_per_iteration(warmup_elapsed, config.warmup_iterations)
    );
    println!(
        "measure_iterations={} tokens_per_build={} total_tokens={}",
        config.iterations, warmup_tokens, total_tokens
    );
    println!(
        "measure_total={measure_elapsed:?} measure_per_iter={:?}",
        duration_per_iteration(measure_elapsed, config.iterations)
    );

    Ok(())
}

fn duration_per_iteration(total: Duration, iterations: usize) -> Duration {
    if iterations == 0 {
        return Duration::ZERO;
    }
    Duration::from_secs_f64(total.as_secs_f64() / iterations as f64)
}

fn parse_args(args: impl Iterator<Item = String>) -> Result<Config, String> {
    let mut sample_path = env::var("ABAP_PERF_SAMPLE")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from(DEFAULT_SAMPLE_PATH));
    let mut iterations = env::var("ABAP_PERF_ITERATIONS")
        .ok()
        .and_then(|raw| raw.parse::<usize>().ok())
        .filter(|value| *value > 0)
        .unwrap_or(20);
    let mut warmup_iterations = env::var("ABAP_PERF_WARMUP")
        .ok()
        .and_then(|raw| raw.parse::<usize>().ok())
        .filter(|value| *value > 0)
        .unwrap_or(1);

    let mut args = args.peekable();
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--sample" => {
                let value = args
                    .next()
                    .ok_or_else(|| "--sample requires a file path".to_string())?;
                sample_path = PathBuf::from(value);
            }
            "--iterations" => {
                let value = args
                    .next()
                    .ok_or_else(|| "--iterations requires a number".to_string())?;
                iterations = parse_positive_usize(&value, "--iterations")?;
            }
            "--warmup" => {
                let value = args
                    .next()
                    .ok_or_else(|| "--warmup requires a number".to_string())?;
                warmup_iterations = parse_positive_usize(&value, "--warmup")?;
            }
            "--help" | "-h" => {
                print_usage();
                std::process::exit(0);
            }
            other => {
                return Err(format!("unrecognized argument: {other}"));
            }
        }
    }

    if !Path::new(&sample_path).exists() {
        return Err(format!(
            "perf sample does not exist: '{}'",
            sample_path.display()
        ));
    }

    Ok(Config {
        sample_path,
        iterations,
        warmup_iterations,
    })
}

fn parse_positive_usize(raw: &str, flag: &str) -> Result<usize, String> {
    raw.parse::<usize>()
        .ok()
        .filter(|value| *value > 0)
        .ok_or_else(|| format!("{flag} expects a positive integer, got '{raw}'"))
}

fn print_usage() {
    println!("Usage: cargo run -p abap_lsp --example build_semantic_tokens_perf -- [options]");
    println!("Options:");
    println!("  --sample <path>       ABAP source file to analyze");
    println!("  --iterations <n>      Measured build iterations (default: 20)");
    println!("  --warmup <n>          Warmup iterations before timing (default: 1)");
    println!("  --help                Show this message");
    println!("Environment:");
    println!("  ABAP_PERF_SAMPLE      Default sample path override");
    println!("  ABAP_PERF_ITERATIONS  Default measured iterations override");
    println!("  ABAP_PERF_WARMUP      Default warmup iterations override");
}
