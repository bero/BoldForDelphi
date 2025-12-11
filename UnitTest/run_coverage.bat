@echo off
REM Bold for Delphi Code Coverage Script
REM Requires DelphiCodeCoverage from https://github.com/DelphiCodeCoverage/DelphiCodeCoverage

set COVERAGE_EXE=C:\Attracs\DelphiCodeCoverage\Win32\CodeCoverage.exe
set TEST_EXE=UnitTest.exe
set MAP_FILE=UnitTest.map
set SOURCE_DIR=..\Source
set OUTPUT_DIR=coverage_report

REM Check if CodeCoverage.exe exists
if not exist "%COVERAGE_EXE%" (
    echo ERROR: CodeCoverage.exe not found at %COVERAGE_EXE%
    echo Please build DelphiCodeCoverage or download a release
    echo See: https://github.com/DelphiCodeCoverage/DelphiCodeCoverage/releases
    pause
    exit /b 1
)

REM Check if map file exists
if not exist "%MAP_FILE%" (
    echo ERROR: %MAP_FILE% not found
    echo Please rebuild UnitTest.dproj with Debug configuration
    pause
    exit /b 1
)

REM Create output directory
if not exist "%OUTPUT_DIR%" mkdir "%OUTPUT_DIR%"

echo Running code coverage analysis...
echo.

"%COVERAGE_EXE%" ^
  -e %TEST_EXE% ^
  -m %MAP_FILE% ^
  -sd %SOURCE_DIR% ^
  -spf coverage_source_paths.lst ^
  -uf coverage_units.lst ^
  -html ^
  -xml ^
  -od %OUTPUT_DIR%

echo.
echo Coverage report generated in %OUTPUT_DIR%
echo Open %OUTPUT_DIR%\CodeCoverage_summary.html to view results
echo.
pause
