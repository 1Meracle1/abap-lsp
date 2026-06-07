@echo off
setlocal

set "ODIN_EXE=D:\dev\odin\toolchain\odin-windows-amd64-dev-2026-05\odin.exe"
set "ODIN_FLAGS=-collection:src=src -vet -warnings-as-errors"
set "TEST_FLAGS="
set "ROOT=%~dp0"

if "%~1"=="" (
    set "TEST_FLAGS=-define:ODIN_TEST_LOG_LEVEL=error"
    goto args_done
)

:parse_args
if "%~1"=="" goto args_done
if /I "%~1"=="--no-leak-warnings" (
    set "TEST_FLAGS=%TEST_FLAGS% -define:ODIN_TEST_LOG_LEVEL=error"
) else (
    set "TEST_FLAGS=%TEST_FLAGS% %1"
)
shift
goto parse_args
:args_done

if not exist "%ROOT%bin" mkdir "%ROOT%bin"

"%ODIN_EXE%" check "%ROOT%src\tokenizer" -no-entry-point %ODIN_FLAGS% || exit /b
"%ODIN_EXE%" check "%ROOT%src\string_interner" -no-entry-point %ODIN_FLAGS% || exit /b
"%ODIN_EXE%" check "%ROOT%src\encoding\toml" -no-entry-point %ODIN_FLAGS% || exit /b
"%ODIN_EXE%" check "%ROOT%src\ast" -no-entry-point %ODIN_FLAGS% || exit /b
"%ODIN_EXE%" check "%ROOT%src\parser" -no-entry-point %ODIN_FLAGS% || exit /b
"%ODIN_EXE%" check "%ROOT%src\execution" -no-entry-point %ODIN_FLAGS% || exit /b
"%ODIN_EXE%" check "%ROOT%src\trace" -no-entry-point %ODIN_FLAGS% || exit /b
"%ODIN_EXE%" check "%ROOT%src\http" -no-entry-point %ODIN_FLAGS% || exit /b
"%ODIN_EXE%" check "%ROOT%src\adt" -no-entry-point %ODIN_FLAGS% || exit /b
"%ODIN_EXE%" check "%ROOT%src\ddic_xml" -no-entry-point %ODIN_FLAGS% || exit /b
"%ODIN_EXE%" check "%ROOT%src\semantic" -no-entry-point %ODIN_FLAGS% || exit /b
"%ODIN_EXE%" check "%ROOT%src\semantic\remote_dependencies" -no-entry-point %ODIN_FLAGS% || exit /b
"%ODIN_EXE%" check "%ROOT%src\remote_dependencies" -no-entry-point %ODIN_FLAGS% || exit /b
"%ODIN_EXE%" check "%ROOT%src\lints" -no-entry-point %ODIN_FLAGS% || exit /b
"%ODIN_EXE%" check "%ROOT%src\persistence\sqlite3" -no-entry-point %ODIN_FLAGS% || exit /b
"%ODIN_EXE%" check "%ROOT%src\dependency_store" -no-entry-point %ODIN_FLAGS% || exit /b
"%ODIN_EXE%" test "%ROOT%src\tokenizer" %ODIN_FLAGS% %TEST_FLAGS% || exit /b
"%ODIN_EXE%" test "%ROOT%src\string_interner" %ODIN_FLAGS% %TEST_FLAGS% || exit /b
"%ODIN_EXE%" test "%ROOT%src\encoding\toml" %ODIN_FLAGS% %TEST_FLAGS% || exit /b
"%ODIN_EXE%" test "%ROOT%src\ast" %ODIN_FLAGS% %TEST_FLAGS% || exit /b
"%ODIN_EXE%" test "%ROOT%src\parser" %ODIN_FLAGS% %TEST_FLAGS% || exit /b
"%ODIN_EXE%" test "%ROOT%src\execution" %ODIN_FLAGS% %TEST_FLAGS% || exit /b
"%ODIN_EXE%" test "%ROOT%src\http" %ODIN_FLAGS% %TEST_FLAGS% || exit /b
"%ODIN_EXE%" test "%ROOT%src\adt" %ODIN_FLAGS% %TEST_FLAGS% || exit /b
"%ODIN_EXE%" test "%ROOT%src\ddic_xml" %ODIN_FLAGS% %TEST_FLAGS% || exit /b
"%ODIN_EXE%" test "%ROOT%src\semantic" %ODIN_FLAGS% %TEST_FLAGS% || exit /b
"%ODIN_EXE%" test "%ROOT%src\semantic\remote_dependencies" %ODIN_FLAGS% %TEST_FLAGS% || exit /b
"%ODIN_EXE%" test "%ROOT%src\remote_dependencies" %ODIN_FLAGS% %TEST_FLAGS% || exit /b
"%ODIN_EXE%" test "%ROOT%src\lints" %ODIN_FLAGS% %TEST_FLAGS% || exit /b
"%ODIN_EXE%" test "%ROOT%src\persistence\sqlite3" %ODIN_FLAGS% %TEST_FLAGS% || exit /b
"%ODIN_EXE%" test "%ROOT%src\dependency_store" %ODIN_FLAGS% %TEST_FLAGS%
