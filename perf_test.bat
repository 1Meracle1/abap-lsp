@echo off
setlocal enabledelayedexpansion
cd /D "%~dp0"

set MODE=debug
set CARGO_ARGS=

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
  
  echo large_file_phase_breakdown
  cargo test -p abap_symbols large_file_phase_breakdown -- --ignored --nocapture --release%CARGO_ARGS%

  echo semantic_tokens_full_request_throughput_smoke
  cargo test -p abap_lsp --test semantic_tokens_perf_smoke semantic_tokens_full_request_throughput_smoke -- --ignored --nocapture --release%CARGO_ARGS%
) else (
  echo [debug mode]

  echo large_file_phase_breakdown
  cargo test -p abap_symbols large_file_phase_breakdown -- --ignored --nocapture%CARGO_ARGS%

  echo semantic_tokens_full_request_throughput_smoke
  cargo test -p abap_lsp --test semantic_tokens_perf_smoke semantic_tokens_full_request_throughput_smoke -- --ignored --nocapture%CARGO_ARGS%
)