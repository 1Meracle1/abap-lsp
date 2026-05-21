@echo off
setlocal enabledelayedexpansion

set "ODIN_EXE=D:\dev\odin\toolchain\odin-windows-amd64-dev-2026-05\odin.exe"
set "ODIN_FLAGS=-vet -warnings-as-errors"
set "ODIN_LINKER_FLAGS=-extra-linker-flags:/STACK:4000000,2000000"
set "ROOT=%~dp0"
set "MODE=debug"
set "ODIN_EXTRA_ARGS="

:parse
if "%~1"=="" goto build
if /I "%~1"=="debug" (
  set "MODE=debug"
) else if /I "%~1"=="release" (
  set "MODE=release"
) else (
  set "ODIN_EXTRA_ARGS=!ODIN_EXTRA_ARGS! %~1"
)
shift
goto parse

:build
set "OUT_DIR=%ROOT%bin\%MODE%"
if not exist "%OUT_DIR%" mkdir "%OUT_DIR%"

if /I "%MODE%"=="release" (
  echo [release mode]
  set "MODE_FLAGS=-o:speed"
) else (
  echo [debug mode]
  set "MODE_FLAGS=-debug"
)

"%ODIN_EXE%" build "%ROOT%cmd\abap_frontend" -out:"%OUT_DIR%\abap_frontend.exe" %ODIN_FLAGS% %ODIN_LINKER_FLAGS% %MODE_FLAGS% %ODIN_EXTRA_ARGS%
