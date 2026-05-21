@echo off
setlocal enabledelayedexpansion

set "ROOT=%~dp0"
set "MODE=debug"

if /I "%~1"=="debug" (
  shift
) else if /I "%~1"=="release" (
  set "MODE=release"
  shift
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
"%ROOT%bin\%MODE%\abap_frontend.exe"%APP_ARGS%
