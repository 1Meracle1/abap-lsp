@echo off
setlocal enabledelayedexpansion

set "ROOT=%~dp0"
set "MODE=debug"
set "APP=abap_frontend"

:parse_prefix
if /I "%~1"=="debug" (
  set "MODE=debug"
  shift
  goto parse_prefix
) else if /I "%~1"=="release" (
  set "MODE=release"
  shift
  goto parse_prefix
) else if /I "%~1"=="abap_frontend" (
  set "APP=abap_frontend"
  shift
  goto parse_prefix
) else if /I "%~1"=="adt_cli" (
  set "APP=adt_cli"
  shift
  goto parse_prefix
) else if /I "%~1"=="adt" (
  set "APP=adt_cli"
  shift
  goto parse_prefix
)

call "%ROOT%build.bat" %MODE%
if errorlevel 1 exit /b %errorlevel%

set "APP_ARGS="
:args
if "%~1"=="" goto run
set "APP_ARGS=!APP_ARGS! "%~1""
shift
goto args

:run
"%ROOT%bin\%MODE%\%APP%.exe"%APP_ARGS%
