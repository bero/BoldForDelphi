# Unit Testing for Bold for Delphi

This document describes how to set up, run, and develop unit tests for Bold for Delphi.

## Prerequisites

### DUnitX Environment Variable

The `$(DUNITX)` system environment variable must be set to the DUnitX installation folder. Set this in:
- **Windows**: System Properties > Environment Variables
- **Delphi IDE**: Tools > Options > IDE > Environment Variables

Example: `DUNITX=C:\DUnitX`

## Overview

Bold for Delphi uses the **DUnitX** testing framework. Tests are located in the `UnitTest/` folder and organized by source area:

```
UnitTest/
├── UnitTest.dpr           # Main test project
├── UnitTest.dproj
├── build.bat              # Command-line build script
├── Code/
│   ├── Common/            # Tests for Common units
│   ├── ObjectSpace/       # Tests for ObjectSpace units
│   ├── FreestandingValueSpace/
│   └── Main/              # Test model classes
└── coverage_report/       # Generated coverage reports
```

## Running Tests

### Command Line

```batch
cd UnitTest
build.bat
UnitTest.exe --exit:Continue
```

Note use pause if you want to pause before exit
```batch
UnitTest.exe --exit:Pause
```


### From Delphi IDE

1. Open `UnitTest.dproj` in Delphi
2. Build (Shift+F9)
3. Run (F9)

## TestInsight

**TestInsight** is a Delphi IDE plugin that enables running and debugging individual tests directly from the IDE. This is highly recommended for test development.

### Installing TestInsight

1. Download from: https://bitbucket.org/sglienke/testinsight/downloads/
2. Run the installer
3. Restart Delphi IDE
4. TestInsight panel appears in View > TestInsight Explorer

### Using TestInsight

1. Open `UnitTest.dproj` in Delphi
2. Open **View > TestInsight Explorer**
3. Build the project (Shift+F9)
4. Tests appear in the TestInsight panel
5. **Run single test**: Click the green arrow next to a test
6. **Debug test**: Right-click > Debug Selected Tests
7. **Run all**: Click Run All in the toolbar

### TestInsight Features

- Run/debug individual tests without running the entire suite
- See test results inline in the IDE
- Double-click failures to jump to the failing line
- Filter tests by name or status
- Re-run failed tests only

### TESTINSIGHT Conditional Define

The `TESTINSIGHT` conditional define controls how the test project runs:

| TESTINSIGHT | Behavior |
|-------------|----------|
| **Defined** | Tests can only run via TestInsight plugin. The exe communicates with TestInsight. |
| **Undefined** | Tests run as a normal console application. Can run/debug from IDE or command line. |

**To toggle TestInsight mode:**

1. **Via menu**: Right-click the project > **Enable TestInsight** (or Disable)
2. **Via project options**: Project > Options > Delphi Compiler > Conditional defines > Add/remove `TESTINSIGHT`

When developing new tests, enable TestInsight for quick iteration. For CI/CD or command-line execution, disable it.

## Writing Tests

### Test Structure

```pascal
unit Test.MyUnit;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestMyUnit = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestSomething;
  end;

implementation

procedure TTestMyUnit.Setup;
begin
  // Initialize before each test
end;

procedure TTestMyUnit.TearDown;
begin
  // Cleanup after each test
end;

procedure TTestMyUnit.TestSomething;
begin
  Assert.AreEqual(Expected, Actual, 'Description');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestMyUnit);

end.
```

### Adding Tests to the Project

1. Create test file in appropriate `Code/` subfolder
2. Add to `UnitTest.dpr` uses clause:
   ```pascal
   Test.MyUnit in 'Code\SubFolder\Test.MyUnit.pas';
   ```

### Common Assertions

```pascal
Assert.AreEqual(Expected, Actual);
Assert.IsTrue(Condition);
Assert.IsFalse(Condition);
Assert.IsNull(Value);
Assert.IsNotNull(Value);
Assert.WillRaise(procedure begin ... end, EExpectedException);
```

---

# Code Coverage

Code coverage analysis helps identify untested code paths. Bold for Delphi uses **DelphiCodeCoverage** for coverage reports.

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

Current coverage baseline (as of February 2026):
- **Total lines:** 57,013
- **Covered lines:** 28,523
- **Overall coverage:** 50%
- **Tests:** 1,631 total (1,604 passing, 27 ignored)

### Top Units by Uncovered Lines (Testable Without Database)

These units have the most untested code and can be tested with in-memory objects only:

| Unit | Total | Covered | Uncovered | Coverage | Notes |
|------|-------|---------|-----------|----------|-------|
| BoldUMLModel.pas | 7,199 | 648 | 6,551 | 9% | Very large; needs UML model setup |
| BoldSystem.pas | 4,797 | 2,632 | 2,165 | 55% | Core object space; partially testable |
| BoldLinks.pas | 1,652 | 576 | 1,076 | 35% | Association handling; needs test model with links |
| BoldAttributes.pas | 2,471 | 1,466 | 1,005 | 59% | Attribute types; many edge cases testable |
| BoldMeta.pas | 1,521 | 960 | 561 | 63% | Runtime metamodel |
| BoldOclSymbolImplementations.pas | 1,757 | 1,234 | 523 | 70% | OCL operations; testable via OCL expressions |
| BoldUndoHandler.pas | 700 | 277 | 423 | 40% | Undo/redo; testable with in-memory system |
| BoldSystemRT.pas | 1,174 | 761 | 413 | 65% | Runtime type info |
| BoldOclVariables.pas | 388 | 0 | 388 | 0% | OCL variable support; needs variable binding tests |
| BoldOcl.pas | 637 | 325 | 312 | 51% | OCL parser; testable via expression evaluation |
| BoldUMLModelValidator.pas | 491 | 201 | 290 | 41% | Model validation rules |
| BoldOclLightWeightNodes.pas | 586 | 316 | 270 | 54% | OCL lightweight nodes |
| BoldOclSemantics.pas | 518 | 296 | 222 | 57% | OCL type checking |
| BoldOclLightWeightNodeMaker.pas | 176 | 0 | 176 | 0% | LW node creation; testable via OCL LW API |

### Top Units Requiring Database

These units require a database connection (FireDAC/InterBase) for testing:

| Unit | Total | Covered | Uncovered | Coverage | Notes |
|------|-------|---------|-----------|----------|-------|
| BoldPMappersDefault.pas | 1,942 | 809 | 1,133 | 42% | Default persistence mappers |
| BoldPMappersAttributeDefault.pas | 692 | 100 | 592 | 14% | Attribute persistence mappers |
| BoldDBInterfaces.pas | 707 | 181 | 526 | 26% | Database abstraction layer |
| BoldSQLQuery.pas | 512 | 0 | 512 | 0% | SQL query generation |
| BoldDbEvolutor.pas | 511 | 0 | 511 | 0% | Database schema evolution |
| BoldPMappersLinkDefault.pas | 675 | 324 | 351 | 48% | Link persistence mappers |
| BoldSqlSymbols.pas | 459 | 109 | 350 | 24% | SQL symbol table |
| BoldFireDACInterfaces.pas | 704 | 470 | 234 | 67% | FireDAC adapter |

### Units at 100% Coverage

These units are fully covered: BoldMemberTypeDictionary, BoldLoggableCriticalSection, BoldDefaultStreamNames, BoldDefs, BoldPMapper, BoldNamedValueList, BoldUMLTypes, BoldPerformanceStub, BoldSSExcept, BoldRev, BoldFreeStandingValueFactories, BoldActionDefs, BoldAbstractObjectUpgraderHandle, BoldHandle, BoldSorter, BoldPMapperLists, BoldSubscribableCollection, BoldTaggedValueList, BoldLogReceiverInterface, BoldThreadSafeQueue, BoldDefaultTaggedValues, BoldIsoDateTime, BoldGUIDUtils, BoldHashIndexes, BoldPSParams, BoldSharedStrings, BoldGuard, BoldCollections, BoldUMLTaggedValues, BoldPersistenceHandleDB, BoldEventQueue.

## Tips for Improving Coverage

1. **Focus on uncovered lines** - Check the coverage report and target specific uncovered code paths
2. **Test edge cases** - Error handling, boundary conditions, exception paths
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
