# Code Coverage Setup for Bold for Delphi Unit Tests

This document describes how to set up and run code coverage analysis for the Bold for Delphi unit tests using DelphiCodeCoverage.

## Prerequisites

1. **DelphiCodeCoverage** - Download from https://github.com/DelphiCodeCoverage/DelphiCodeCoverage/releases
2. **Delphi IDE** with Debug configuration that generates MAP files
3. **UnitTest project** built with MAP file generation enabled

## Setup Steps

### Step 1: Download DelphiCodeCoverage

1. Go to https://github.com/DelphiCodeCoverage/DelphiCodeCoverage/releases
2. Download the latest release (e.g., `CodeCoverage-x.x.zip`)
3. Extract to a folder, e.g., `C:\DelphiCodeCoverage`
4. Verify `CodeCoverage.exe` exists in the extraction folder

### Step 2: Enable MAP File Generation

The UnitTest.dproj must generate a detailed MAP file for coverage analysis.

1. Open UnitTest.dproj in Delphi IDE
2. Go to **Project > Options > Delphi Compiler > Linking**
3. Set **Map file** to `Detailed` (or value `3`)
4. Alternatively, ensure this line exists in UnitTest.dproj under PropertyGroup for Debug:
   ```xml
   <DCC_MapFile>3</DCC_MapFile>
   ```

### Step 3: Build the Project

Build the UnitTest project in Debug configuration.

**Option A: Using Delphi IDE (recommended)**
1. Open `UnitTest\UnitTest.dproj` in Delphi
2. Set configuration to **Debug** and platform to **Win32**
3. Build the project (Shift+F9)

**Option B: Using command line**
```batch
cd UnitTest
msbuild UnitTest.dproj /p:Config=Debug /p:Platform=Win32
```
Note: Ensure Delphi's bin folder is in PATH, or run `rsvars.bat` from your Delphi installation first.

Verify that `UnitTest.map` is generated in the `UnitTest` folder (should be ~16MB for detailed MAP).

### Step 4: Configure Coverage Files

Three configuration files are needed in the `UnitTest` folder:

#### coverage_units.lst
Lists all source units to analyze. One unit name per line (without .pas extension):

```
BoldSystem
BoldAttributes
BoldElements
BoldObjectList
...
```

To generate this list from the Source folder:
```batch
dir /s /b ..\Source\*.pas | findstr /v "\.inc$" > temp.txt
```
Then extract just the unit names (without path and extension).

#### coverage_source_paths.lst
Lists all source directories to search for .pas files:

```
..\Source\Common\Core
..\Source\Common\Subscription
..\Source\Common\Support
..\Source\ObjectSpace\Core
..\Source\ObjectSpace\BORepresentation
..\Source\Persistence\Core
..\Source\PMapper\SQL
...
```

#### run_coverage.bat
Batch file to execute coverage analysis:

```batch
@echo off
REM Bold for Delphi Code Coverage Script

set COVERAGE_EXE=C:\DelphiCodeCoverage\CodeCoverage.exe
set TEST_EXE=UnitTest.exe
set MAP_FILE=UnitTest.map
set SOURCE_DIR=..\Source
set OUTPUT_DIR=coverage_report

REM Check if CodeCoverage.exe exists
if not exist "%COVERAGE_EXE%" (
    echo ERROR: CodeCoverage.exe not found at %COVERAGE_EXE%
    echo Please download from https://github.com/DelphiCodeCoverage/DelphiCodeCoverage/releases
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
```

## Running Code Coverage

1. Open a command prompt in the `UnitTest` folder
2. Run:
   ```batch
   run_coverage.bat
   ```
3. The tests will execute with coverage instrumentation
4. Results are generated in the `coverage_report` folder

## Viewing Results

Open `coverage_report\CodeCoverage_summary.html` in a web browser.

The report shows:
- **Overall coverage** - Total lines covered vs total lines
- **Per-unit coverage** - Click on any unit to see line-by-line coverage
- **Green lines** - Executed during tests
- **Red lines** - Not executed during tests

## Coverage Metrics

Current coverage baseline (as of December 2025):
- **Total lines:** ~60,000
- **Covered lines:** ~15,500
- **Overall coverage:** ~26%

Key units:
| Unit | Coverage |
|------|----------|
| BoldSystem.pas | 44.6% |
| BoldAttributes.pas | ~50% |
| BoldElements.pas | ~40% |

## Tips for Improving Coverage

1. **Add more unit tests** - Each test exercises code paths
2. **Test edge cases** - Error handling, boundary conditions
3. **Use persistence tests** - Many code paths require database operations
4. **Test with different configurations** - Transient vs persistent mode

## Troubleshooting

### "MAP file not found"
- Rebuild UnitTest.dproj in Debug configuration
- Verify `DCC_MapFile=3` in project options

### "CodeCoverage.exe not found"
- Download DelphiCodeCoverage from GitHub releases
- Update `COVERAGE_EXE` path in run_coverage.bat

### Coverage shows 0% for a unit
- Verify the unit is listed in coverage_units.lst
- Verify the source path is in coverage_source_paths.lst
- Check that the unit is actually linked into UnitTest.exe

### Tests fail during coverage run
- Coverage adds overhead; some timing-sensitive tests may fail
- Run tests normally first to verify they pass
- Consider increasing timeouts for flaky tests

## Command Line Options

DelphiCodeCoverage supports many options:

| Option | Description |
|--------|-------------|
| `-e <exe>` | Executable to run |
| `-m <map>` | MAP file path |
| `-sd <dir>` | Source directory |
| `-spf <file>` | Source paths file |
| `-uf <file>` | Units file |
| `-html` | Generate HTML report |
| `-xml` | Generate XML report |
| `-od <dir>` | Output directory |
| `-lt <threshold>` | Line coverage threshold |
| `-mc <threshold>` | Method coverage threshold |

For full documentation, see: https://github.com/DelphiCodeCoverage/DelphiCodeCoverage
