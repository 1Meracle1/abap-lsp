@echo off
setlocal

set "ODIN_EXE=D:\dev\odin\toolchain\odin-windows-amd64-dev-2026-05\odin.exe"
set "ODIN_FLAGS=-default-to-panic-allocator -vet -warnings-as-errors"
set "ROOT=%~dp0"

if not exist "%ROOT%bin" mkdir "%ROOT%bin"

"%ODIN_EXE%" run "%ROOT%cmd\abap_frontend" %ODIN_FLAGS% -- %*
