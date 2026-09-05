# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Delphi/Bold Framework Conventions
- This is a Delphi codebase using the Bold for Delphi framework. When editing Delphi code, always check that accessed properties/methods are public or published (not protected/private). Verify correct unit imports (e.g., System.SysUtils) before finishing edits.
- Bold OCL expressions have specific syntax — do not guess OCL chains. Ask the user or search the model files for the correct OCL path before making changes.

## Project Overview

Bold for Delphi is a Model-Driven Architecture (MDA) framework and Object-Relational Mapping (ORM) tool. It enables UML-based model development with OCL (Object Constraint Language) queries, automatic code generation, and sophisticated database persistence. Originally released by Boldsoft in 2004, it was open-sourced by Embarcadero in 2020 under MIT license.

**Current Version**: 4.0.1.0 (community-maintained develop branch)
**Target Platforms**: Delphi 11.3 Alexandria, 12.3 Athens and 13.1 Florence (Win32/Win64)

## Claude Code Instructions

By default never guess to generate the answer. If information is missing ask for clarification. Only guess if the prompt actually tells you to.

### Important Paths (REMEMBER THESE!)

- **UnitTest.exe**: `C:\Attracs\BoldForDelphi\UnitTest\UnitTest.exe`
- **UnitTest project folder**: `C:\Attracs\BoldForDelphi\UnitTest`

### Testing Strategy for Bold Source Changes

When modifying any code in `Source/`, follow this workflow:

#### 1. Check Coverage First
Before making changes, check if the method(s) you're modifying have 100% test coverage.
- Look at `UnitTest/coverage_report/CodeCoverage_summary.html`
- If already 100% covered, proceed to step 4

#### 2. Write Test First (if not fully covered)
Create a unit test that covers the code you're about to change:
- For **refactoring**: Write a test that passes with current code
- For **bugfix**: Write a test that FAILS with current code (demonstrates the bug)
- For bugfixes, confirm the RED baseline with the user before editing production code

#### 3. Building Unit Tests (MANDATORY - USE EXACTLY)

**Use the build.ps1 script** - it properly sets DUnitX and DelphiMocks environment variables:

```powershell
powershell -ExecutionPolicy Bypass -File "C:\Attracs\BoldForDelphi\UnitTest\build.ps1"
```

**Why use build.ps1?**
- Sets `$env:DUnitX` and `$env:DelphiMocks` correctly
- Avoids "Unit 'DUnitX.Loggers.Console' not found" errors
- Works reliably from bash shell

**DO NOT use inline env var setting** - it fails due to bash-to-PowerShell transition issues:
```powershell
# WRONG - env vars get mangled
powershell -Command "& { $env:DUnitX = '...'; ... }"
```

### Running Specific Tests

The `--run` filter uses the full namespace path `UnitName.ClassName[.TestName]`.

**IMPORTANT**: On Windows PowerShell, use this exact syntax to capture all output:

```powershell
# From project root (C:\Attracs\BoldForDelphi)
powershell -Command "& '.\UnitTest\UnitTest.exe' --run:Test.BoldUndoHandler.TTestBoldUndoHandler --consolemode:Verbose 2>&1"

# Run ALL tests in a test class
powershell -Command "& '.\UnitTest\UnitTest.exe' --run:Test.BoldPMappersDefault.TTestBoldPMappersDefault 2>&1"

# Run ONE specific test
powershell -Command "& '.\UnitTest\UnitTest.exe' --run:Test.BoldPMappersDefault.TTestBoldPMappersDefault.TestMockQueryWithBoldDbTypeField 2>&1"

# Run with quiet output
powershell -Command "& '.\UnitTest\UnitTest.exe' --run:Test.BoldDBInterfacesMock.TTestBoldDBInterfacesMock --consolemode:Quiet 2>&1"

# Run with verbose output (recommended for debugging)
powershell -Command "& '.\UnitTest\UnitTest.exe' --run:Test.BoldUndoHandler.TTestBoldUndoHandler --consolemode:Verbose 2>&1"
```

**Why this syntax?**
- `powershell -Command "& '...' 2>&1"` ensures both stdout and stderr are captured
- Use single quotes around the exe path for paths with spaces
- Always run from project root with `.\UnitTest\UnitTest.exe` path

#### 4. Make the Source Change
Now implement your refactoring or bugfix in Bold source.

#### 5. Run Full Coverage
```powershell
powershell -ExecutionPolicy Bypass -File "C:\Attracs\BoldForDelphi\UnitTest\run_coverage.ps1"
```

#### 6. Verify
- All tests pass
- Changed code is now covered in the coverage report

#### 7. Upload Coverage to Codecov.io (optional)
Use the `/coverage-cycle` skill — it covers conversion, upload, and the README statistics update.

**Summary**: Test-first for bugfixes (test fails then passes), test-first for refactoring (test passes before and after).

### Git Commit Messages

Do not include Claude references in git commit messages. No "Generated with Claude", no "Co-Authored-By: Claude", and no similar attributions.

### File Encoding

- **New Delphi units**: Always use UTF-8 with BOM
- **Existing units**: Convert to UTF-8 with BOM when modifying the file

### Writing Unit Tests

When adding unit tests to improve code coverage:

1. **Focus on uncovered lines** - Check the coverage report first. If a unit has 85/100 lines covered, write tests targeting the remaining 15 uncovered lines, not the already-covered code.

2. **Minimal tests for coverage** - Write the minimum test code needed to exercise uncovered paths. Don't add redundant tests for code already covered by existing tests.

3. **Check what's already tested** - Before writing tests, understand why existing coverage exists. Often, internal functions are exercised indirectly through other code paths.

4. **Target exception paths** - Uncovered lines are often error handling or edge cases. These typically require specific test scenarios (invalid input, null values, etc.).

5. **One test per gap** - If only one function/path is uncovered, one focused test is sufficient.

Example: If `BoldFoo.pas` has 97% coverage with only an exception handler uncovered, write a single test that triggers that exception - don't write additional tests for the already-covered happy paths.

### Keeping README.md Updated

When adding or modifying unit tests, update README.md's test count and coverage statistics — the `/coverage-cycle` skill includes the full procedure.

## Project Structure

### Folders to Ignore

- **Source/Deprecated/** - Contains deprecated database adapters (ADO, BDE, Advantage, DBExpress). Not used, not maintained. Ignore when searching for cleanup opportunities.

## Build Commands for Delphi projects and packages

- PowerShell script `C:\Attracs\DelphiStandards\DelphiBuildDPROJ.ps1`
This script in Powershell detect latest Delphi version and use that.
-DelphiVersion override used Delphi.
Read comments in the script for all details.

Example: Build MasterDetail project With latest Delphi (Delphi 13) and when Current folder is `C:\Attracs\boldfordelphi`

`C:\Attracs\DelphiStandards\DelphiBuildDPROJ.ps1 -Projectfile "examples\Simple\ObjectSpace\MasterDetail\MasterDetail.dproj" -DelphiVersion "37.0" -VerboseOutPut`

Example: Build Bold package for Delphi 12.3

`C:\Attracs\DelphiStandards\DelphiBuildDPROJ.ps1 -Projectfile "packages\Delphi12.3\dclBold.dproj" -DelphiVersion "23.0" -VerboseOutPut`

## Environment Setup

Core Bold packages use **relative paths** and require no environment variable setup. Clone/copy the repository to any location and build.

**Optional**: For UniDAC support, set the `UniDAC` environment variable in Delphi (Tools > Options > Environment Variables) pointing to the UniDAC installation root.

### UniDAC tests (optional - UniDAC is commercial and never required)

The default `Debug`/`Release` test builds compile empty stub twins of the UniDAC test units
(`UnitTest\Code\Persistence\UniDACStubs`), found through the unit search path - the same idea as
`Source\Common\Stubs`. No `$IFDEF` is involved, neither in code nor in the dpr. The build
configuration `DebugUniDAC` puts the real units (`UnitTest\Code\Persistence\UniDAC`) and
`$(UniDAC)\Source` first on the path instead, and writes its exe to `UnitTest\UniDAC\`:

```powershell
# UniDAC 10.4 does not compile with Delphi 13 - pin Delphi 12
powershell -ExecutionPolicy Bypass -File "C:\Attracs\BoldForDelphi\UnitTest\build.ps1" -Config DebugUniDAC -DelphiVersion 23.0
# UnitTest.ini must say Engine=SQLServer (that UniDAC has no SQLite provider); restore it afterwards
powershell -Command "& '.\UnitTest\UniDAC\UnitTest.exe' --run:Test.PersistenceUniDAC 2>&1"
```

`build.ps1` defaults `$env:UniDAC` to `C:\Attracs\Attracs-Common\components\UniDAC` when the variable
is not set. A new UniDAC-only test unit needs an empty twin with the same unit name in `UniDACStubs`.

## Compiler Directives (Bold.inc)

The `{$DEFINE Attracs}` block at end of Bold.inc enables optimizations used by Attracs. When `Attracs` is NOT defined, several features are disabled for broader compatibility.

Key defines (enabled when `Attracs` is defined):
```pascal
{$DEFINE SpanFetch}                          // Efficient batch fetching
{$DEFINE IDServer}                           // External ID server (improves write perf)
{$DEFINE CompareToOldValues}                 // Skip unchanged values in updates
{$DEFINE NoNegativeDates}                    // Restrict date range validation
{$DEFINE NoTransientInstancesOfPersistentClass}  // Performance optimization
```

Other notable defines:
```pascal
{$DEFINE BoldJson}           // JSON serialization support (always on)
{$DEFINE BOLD_NO_QUERIES}    // Turns off query mechanism (always on)
```

## Key Concepts

### Subscription Pattern
Bold uses a sophisticated subscription system for automatic UI updates when objects change. Components subscribe to objects/attributes and receive notifications on changes.

## Resources

- Wiki: https://delphi.fandom.com/wiki/Bold_for_Delphi
- Blog: http://boldfordelphi.blogspot.com/
- Discord: https://discord.gg/C6frzsn
