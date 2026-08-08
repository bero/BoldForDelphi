# Bold for Delphi - Unit Tests

This folder contains the DUnitX test suite for Bold for Delphi
(2100+ tests). **The test projects require two external frameworks that
are not part of this repository** - if you just tried to build
`UnitTest.dproj` and got

```
F2613 Unit 'DUnitX.Loggers.Console' not found
F2613 Unit 'Delphi.Mocks' not found
```

this README is for you. Nothing in `Source/` needs these frameworks;
they are test-only dependencies.

## Dependencies

| Dependency | Environment variable | Purpose |
|------------|---------------------|---------|
| [DUnitX](https://github.com/VSoftTechnologies/DUnitX) | `DUnitX` | Unit testing framework |
| [Delphi-Mocks](https://github.com/VSoftTechnologies/Delphi-Mocks) | `DelphiMocks` | Mocking framework for interface testing |

The `.dproj` search paths reference them as `$(DUnitX)` and
`$(DelphiMocks)`. Each variable must point to the framework's **Source**
subfolder.

## Setup

### Option A: setup script (recommended)

```powershell
cd UnitTest
.\setup-tests.ps1
```

This clones both frameworks next to the repository and sets the two
environment variables (user level). **Restart the Delphi IDE afterwards** -
it reads environment variables at startup.

### Option B: manual (no Git required)

1. Get the frameworks, either with Git:
   ```
   git clone https://github.com/VSoftTechnologies/DUnitX.git
   git clone https://github.com/VSoftTechnologies/Delphi-Mocks.git
   ```
   or without Git — download and extract the ZIPs:
   - https://github.com/VSoftTechnologies/DUnitX/archive/refs/heads/master.zip
   - https://github.com/VSoftTechnologies/Delphi-Mocks/archive/refs/heads/master.zip

   (A ZIP extracts to a folder named `DUnitX-master` / `Delphi-Mocks-master` —
   rename or just point the variables at the actual folder.)
2. Set the variables, either as Windows user environment variables or in
   the Delphi IDE under **Tools > Options > IDE > Environment Variables**.
   Each variable must point to the framework's `Source` subfolder:
   - `DUnitX` = `<path>\DUnitX\Source`
   - `DelphiMocks` = `<path>\Delphi-Mocks\Source`
3. Restart the IDE — it reads environment variables at startup.
4. Open `UnitTest.groupproj` and build.

## Building and Running

**In the IDE:** open `UnitTest.groupproj` (contains the console runner
`UnitTest.dproj` and the GUI runner `UnitTestGUI.dproj`) and build.

**Command line:**

```powershell
.\build.ps1                  # console runner, Debug/Win32
.\build.ps1 -GUI             # GUI runner
.\UnitTest.exe               # run all tests
```

Run a specific fixture or test:

```powershell
.\UnitTest.exe --run:Test.BoldUndoHandler.TTestBoldUndoHandler --consolemode:Verbose
```

## Code Coverage

```powershell
.\run_coverage.ps1
```

Builds, runs all tests under
[DelphiCodeCoverage](https://github.com/DelphiCodeCoverage/DelphiCodeCoverage)
(expected at `C:\Attracs\DelphiCodeCoverage\`), and opens the HTML report
from `coverage_report/`. See the main [README](../README.md) for current
statistics and Codecov upload.
