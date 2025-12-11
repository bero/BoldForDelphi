@echo off
setlocal enabledelayedexpansion

REM Find the latest installed Delphi version
set "STUDIO_BASE=C:\Program Files (x86)\Embarcadero\Studio"
set "LATEST_VERSION="

if not exist "%STUDIO_BASE%" (
    echo Error: Cannot find Embarcadero Studio installation
    exit /b 1
)

pushd "%STUDIO_BASE%"
for /d %%d in (*) do if exist "%%d\bin\rsvars.bat" set "LATEST_VERSION=%%d"
popd

if "!LATEST_VERSION!"=="" (
    echo Error: No Delphi installation found
    exit /b 1
)

echo Using Delphi installation: %STUDIO_BASE%\!LATEST_VERSION!
call "%STUDIO_BASE%\!LATEST_VERSION!\bin\rsvars.bat"
msbuild "%~dp0UnitTest.dproj" /p:Config=Debug /p:Platform=Win32 /v:minimal
