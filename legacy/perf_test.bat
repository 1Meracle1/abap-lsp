@echo off
setlocal enabledelayedexpansion
cd /D "%~dp0"

set MODE=debug
set CARGO_ARGS=
set CARGO_TEST_PROFILE=

:parse
if "%~1"=="" goto run
if /I "%~1"=="release" (
  set MODE=release
) else (
  set CARGO_ARGS=!CARGO_ARGS! %~1
)
shift
goto parse

:run
if /I "%MODE%"=="release" (
  echo [release mode]
  set CARGO_TEST_PROFILE=--release
) else (
  echo [debug mode]
)

echo perf_baseline_json
if /I "%MODE%"=="release" (
  cargo run -p abap_cli --example perf_baseline --release --%CARGO_ARGS%
) else (
  cargo run -p abap_cli --example perf_baseline --%CARGO_ARGS%
)

set PERF_CORPUS=%CD%\examples\perf_corpus
set LOCAL_EXPORT_WORKSPACE=%PERF_CORPUS%\local-export-workspace
set LOCAL_EXPORT_TARGET=%LOCAL_EXPORT_WORKSPACE%\src\ZPERF_LOCAL_DRIVER.abap

echo workspace_analysis_perf_local_export_corpus
if /I "%MODE%"=="release" (
  cargo run -p abap_cache --example workspace_analysis_perf --release -- --workspace "%LOCAL_EXPORT_WORKSPACE%" --target "%LOCAL_EXPORT_TARGET%"
) else (
  cargo run -p abap_cache --example workspace_analysis_perf -- --workspace "%LOCAL_EXPORT_WORKSPACE%" --target "%LOCAL_EXPORT_TARGET%"
)

echo workspace_local_export_perf_corpus
if /I "%MODE%"=="release" (
  cargo run -p abap_lsp --example workspace_local_export_perf --release -- --workspace "%LOCAL_EXPORT_WORKSPACE%" --target "%LOCAL_EXPORT_TARGET%"
) else (
  cargo run -p abap_lsp --example workspace_local_export_perf -- --workspace "%LOCAL_EXPORT_WORKSPACE%" --target "%LOCAL_EXPORT_TARGET%"
)

if defined ABAP_PERF_SAMPLE (
  if exist "%ABAP_PERF_SAMPLE%" (
    echo large_file_phase_breakdown
    cargo test %CARGO_TEST_PROFILE% -p abap_symbols large_file_phase_breakdown -- --ignored --nocapture%CARGO_ARGS%
  ) else (
    echo skipping large_file_phase_breakdown: ABAP_PERF_SAMPLE does not exist: %ABAP_PERF_SAMPLE%
  )
) else (
  echo skipping large_file_phase_breakdown: set ABAP_PERF_SAMPLE to a large ABAP source file
)

echo parser_throughput_smoke
cargo test %CARGO_TEST_PROFILE% -p abap_parser --test perf_smoke parser_throughput_smoke -- --ignored --nocapture%CARGO_ARGS%

echo semantic_tokens_full_request_throughput_smoke
cargo test %CARGO_TEST_PROFILE% -p abap_lsp --test semantic_tokens_perf_smoke semantic_tokens_full_request_throughput_smoke -- --ignored --nocapture%CARGO_ARGS%

if defined ABAP_PERF_SAMPLE (
  if exist "%ABAP_PERF_SAMPLE%" (
    echo build_semantic_tokens_perf
    if /I "%MODE%"=="release" (
      cargo run -p abap_lsp --example build_semantic_tokens_perf --release --%CARGO_ARGS%
    ) else (
      cargo run -p abap_lsp --example build_semantic_tokens_perf --%CARGO_ARGS%
    )
  ) else (
    echo skipping build_semantic_tokens_perf: ABAP_PERF_SAMPLE does not exist: %ABAP_PERF_SAMPLE%
  )
) else (
  echo skipping build_semantic_tokens_perf: set ABAP_PERF_SAMPLE to a large ABAP source file
)
