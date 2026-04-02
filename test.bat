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
  cargo test --workspace --release%CARGO_ARGS%
) else (
  echo [debug mode]
  cargo test --workspace%CARGO_ARGS%
)
