@echo off
setlocal

set "ODIN_EXE=D:\dev\odin\toolchain\odin-windows-amd64-dev-2026-05\odin.exe"
set "ODIN_FLAGS=-default-to-panic-allocator -vet -warnings-as-errors"
set "ROOT=%~dp0"

if not exist "%ROOT%bin" mkdir "%ROOT%bin"

"%ODIN_EXE%" check "%ROOT%src\tokenizer" -no-entry-point %ODIN_FLAGS% || exit /b
"%ODIN_EXE%" check "%ROOT%src\ast" -no-entry-point %ODIN_FLAGS% || exit /b
"%ODIN_EXE%" check "%ROOT%src\parser" -no-entry-point %ODIN_FLAGS% || exit /b
"%ODIN_EXE%" test "%ROOT%src\tokenizer" -vet -warnings-as-errors %* || exit /b
"%ODIN_EXE%" test "%ROOT%src\parser" -vet -warnings-as-errors %*
