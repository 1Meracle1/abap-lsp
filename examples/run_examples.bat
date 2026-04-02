@echo off
setlocal EnableDelayedExpansion
cd /D "%~dp0.."
for %%F in ("%~dp0*.abap") do (
  set "NAME=%%~nF"
  if /i "!NAME:~0,9!" neq "negative_" (
    echo ===== %%~nxF =====
    cargo run -p abap_cli -q -- --json check "%%F"
    echo.
  )
)
