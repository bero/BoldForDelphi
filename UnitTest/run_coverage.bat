@echo off
REM ==============================================================================
REM Bold for Delphi - Code Coverage Script
REM ==============================================================================

set COVERAGE_EXE=C:\Attracs\DelphiCodeCoverage\Win32\CodeCoverage.exe
set PROJECT_NAME=UnitTest
set EXECUTABLE=%PROJECT_NAME%.exe
set MAP_FILE=%PROJECT_NAME%.map
set SOURCE_DIR=..\Source
set OUTPUT_DIR=coverage_report

REM --- 1. Check for CodeCoverage Tool ---

if not exist "%COVERAGE_EXE%" (
    echo [ERROR] CodeCoverage.exe not found at: %COVERAGE_EXE%
    echo Please download it from: https://github.com/DelphiCodeCoverage/DelphiCodeCoverage/releases
    pause
    exit /b 1
)

REM --- 2. Setup Delphi Environment (MSBuild) ---

REM A. Check if MSBuild is already working (e.g. RAD Studio Command Prompt)
where msbuild >nul 2>nul
if %errorlevel% equ 0 goto :BuildStart

echo [INFO] MSBuild not in PATH. Searching for Delphi environment...

REM B. Check if BDS variable is manually set or available
if defined BDS (
    if exist "%BDS%\bin\rsvars.bat" (
        echo [INFO] Found BDS variable. Initializing...
        call "%BDS%\bin\rsvars.bat"
        goto :BuildStart
    )
)

REM C. Fallback: Search default installation paths (Newest to Oldest)

REM Delphi 13
if exist "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat" (
    echo [INFO] Found Delphi 13. Initializing...
    call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
    goto :BuildStart
)
REM Delphi 12 (Athens)
if exist "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat" (
    echo [INFO] Found Delphi 12. Initializing...
    call "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"
    goto :BuildStart
)
REM Delphi 11 (Alexandria)
if exist "C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat" (
    echo [INFO] Found Delphi 11. Initializing...
    call "C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat"
    goto :BuildStart
)

echo.
echo [ERROR] Could not find Delphi installation.
echo Please run this script from the "RAD Studio Command Prompt"
echo or manually set the BDS environment variable.
pause
exit /b 1

:BuildStart
REM --- 3. Build the Project ---

echo.
echo [BUILD] Rebuilding %PROJECT_NAME%.dproj...
msbuild %PROJECT_NAME%.dproj /t:Build /p:Config=Debug /p:Platform=Win32 /p:DCC_Define="DEBUG" /p:DCC_MapFile=3 /v:minimal

if %errorlevel% neq 0 (
    echo.
    echo [ERROR] Build failed.
    pause
    exit /b 1
)

REM Verify MAP file was actually generated
if not exist "%MAP_FILE%" (
    echo.
    echo [ERROR] Build succeeded, but %MAP_FILE% is missing.
    echo         Check output paths in Project Options.
    pause
    exit /b 1
)

REM --- 4. Run Coverage Analysis ---

if not exist "%OUTPUT_DIR%" mkdir "%OUTPUT_DIR%"

echo.
echo [COVERAGE] Analyzing code coverage...
echo.

"%COVERAGE_EXE%" ^
  -e %EXECUTABLE% ^
  -m %MAP_FILE% ^
  -sd %SOURCE_DIR% ^
  -spf coverage_source_paths.lst ^
  -uf coverage_units.lst ^
  -html ^
  -xml ^
  -od %OUTPUT_DIR%

echo.
echo ========================================================
echo  Report generated: %OUTPUT_DIR%\CodeCoverage_summary.html
echo ========================================================
echo.

REM Open the report in default browser
start "" "%OUTPUT_DIR%\CodeCoverage_summary.html"
