use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::time::{Duration, Instant};

use abap_parser::parse;
use abap_symbols::perf_api::collect_unit_only;

const DEFAULT_SAMPLE_PATH: &str = r"D:\dev\abap\prod_rep_check\perf-samples\CL_GUI_ALV_GRID.abap";
const PERF_SAMPLE_URI: &str = "file:///symbols_collect_perf_sample.abap";

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
    let source = fs::read_to_string(&config.sample_path).map_err(|err| {
        format!(
            "failed to read perf sample '{}': {err}",
            config.sample_path.display()
        )
    })?;
    let line_count = source.lines().count();

    let parse_start = Instant::now();
    let parsed = parse(&source);
    let parse_elapsed = parse_start.elapsed();

    let warmup_start = Instant::now();
    let mut warmup_unit = collect_unit_only(PERF_SAMPLE_URI, &source, &parsed);
    for _ in 1..config.warmup_iterations {
        warmup_unit = collect_unit_only(PERF_SAMPLE_URI, &source, &parsed);
    }
    let warmup_elapsed = warmup_start.elapsed();

    let measure_start = Instant::now();
    let mut total_symbols = 0usize;
    let mut total_references = 0usize;
    for _ in 0..config.iterations {
        let unit = collect_unit_only(PERF_SAMPLE_URI, &source, &parsed);
        total_symbols += unit.symbols.len();
        total_references += unit.references.len();
    }
    let measure_elapsed = measure_start.elapsed();

    println!(
        "symbols collect perf sample: {}",
        config.sample_path.display()
    );
    println!(
        "bytes={} lines={} tokens={} parse_errors={}",
        source.len(),
        line_count,
        parsed.tokens.len(),
        parsed.errors.len()
    );
    println!("parse={parse_elapsed:?}");
    println!(
        "warmup_iterations={} warmup_total={warmup_elapsed:?} warmup_per_iter={:?}",
        config.warmup_iterations,
        duration_per_iteration(warmup_elapsed, config.warmup_iterations)
    );
    println!(
        "measure_iterations={} measure_total={measure_elapsed:?} measure_per_iter={:?}",
        config.iterations,
        duration_per_iteration(measure_elapsed, config.iterations)
    );
    println!(
        "symbols_per_build={} references_per_build={} diagnostics_per_build={}",
        warmup_unit.symbols.len(),
        warmup_unit.references.len(),
        warmup_unit.diagnostics.len()
    );
    println!(
        "total_symbols={} total_references={}",
        total_symbols, total_references
    );

    Ok(())
}

fn duration_per_iteration(total: Duration, iterations: usize) -> Duration {
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
            other => return Err(format!("unrecognized argument: {other}")),
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
    println!("Usage: cargo run -p abap_symbols --example build_symbols_collect_perf -- [options]");
    println!("Options:");
    println!("  --sample <path>       ABAP source file to analyze");
    println!("  --iterations <n>      Measured collect iterations (default: 20)");
    println!("  --warmup <n>          Warmup iterations before timing (default: 1)");
    println!("Environment:");
    println!("  ABAP_PERF_SAMPLE      Default sample path override");
    println!("  ABAP_PERF_ITERATIONS  Default measured iterations override");
    println!("  ABAP_PERF_WARMUP      Default warmup iterations override");
}
