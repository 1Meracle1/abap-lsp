@echo off
setlocal enabledelayedexpansion
cd /D "%~dp0"
:restart

:: --- Unpack Arguments -------------------------------------------------------
for %%a in (%*) do set "%%~a=1"

if not "%release%"=="1" set debug=1
if "%debug%"=="1"   set release=0 && echo [debug mode]
if "%release%"=="1" set debug=0 && echo [release mode]

if not exist bin           mkdir bin
if not exist "bin/debug"   mkdir "bin/debug"
if not exist "bin/release" mkdir "bin/release"

set flags=
if "%debug%"=="1"   set flags=-o:none -debug
if "%release%"=="1" set flags=-o:speed

set out=
if "%debug%"=="1"   set out=bin/debug/
if "%release%"=="1" set out=bin/release/

if "%abap-lsp%"=="1" odin build src\abap-lsp %flags% -out:%out%abap-lsp.exe || exit /b 1
if "%testbed%"=="1"  odin build src\testbed  %flags% -out:%out%testbed.exe || exit /b 1
