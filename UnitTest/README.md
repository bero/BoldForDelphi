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
.\build.ps1                  # console runner, Debug/Win64
.\build.ps1 -Platform Win32  # the 32-bit runner
.\build.ps1 -GUI             # GUI runner
.\UnitTest.exe               # run all tests
```

Run a specific fixture or test:

```powershell
.\UnitTest.exe --run:Test.BoldUndoHandler.TTestBoldUndoHandler --consolemode:Verbose
```

## VCL-free runtime check

The Bold runtime (object space, OCL, handles, persistence, XML streaming) must
build without the VCL, for console and service applications now and for FMX and
Free Pascal later (#117). `examples\Compound\Invoicing\Console` is that build,
with `FrameworkType=None`: the Bold core links no GUI framework, and a VCL
application links `BoldEnvironmentVCL` itself.
`check_no_vcl.ps1` builds it with a detailed map file and fails when any `Vcl.*`
unit is linked, naming the units; with `-Run` it also runs the 101-check scenario.
Run it after a change under `Source`:

```powershell
.\check_no_vcl.ps1          # build Debug/Win64, check the map file
.\check_no_vcl.ps1 -Run     # ... and run the scenario against ..\Model\Invoicing.bld
```

## Free Pascal / Lazarus (Linux)

Most of the suite also builds with Free Pascal through `FPC/build.sh`. The
FPCUnit runner is `FPC/UnitTestFPC.lpr`, and the test units are shared with
Delphi through `Code/Common/BoldTestFramework.pas` (GitHub issue #117).

### Dependencies: Zeos is mandatory

| Dependency | Environment variable | Purpose |
|------------|---------------------|---------|
| [Free Pascal](https://www.freepascal.org/) 3.2.2 or later | - | Compiler |
| [Zeos](https://sourceforge.net/projects/zeoslib/) 8.0 | `ZEOS` | Database access: under Free Pascal Bold uses its Zeos adapter, under Delphi FireDAC |

**Zeos is mandatory for the Free Pascal build.** `build.sh` stops with an
error when `ZEOS` is not set; there is no stub, unlike UniDAC on Delphi. Zeos
is free (LGPL with a static linking exception), so every Free Pascal run
includes the database tests. `ZEOS` points to the Zeos root folder, the one
that contains `src/`. The SQLite tests also need the SQLite library
(`libsqlite3.so.0`, package `libsqlite3-0` on Debian and Ubuntu).

### Setup and running

```sh
# zeosdbo-8.0.0-stable.zip from
# https://sourceforge.net/projects/zeoslib/files/Zeos%20Database%20Objects/zeosdbo-8.0.0-stable/
unzip zeosdbo-8.0.0-stable.zip -d ~/components/zeosdbo-8.0.0
export ZEOS=~/components/zeosdbo-8.0.0

sh FPC/build.sh                                        # build, then run all tests
sh FPC/build.sh --suite=TTestBoldSystem --format=plain # one fixture
```

`build.sh` compiles Zeos first, in mode Delphi as its Lazarus packages do,
into `FPC/bin/zeos`, and then the tests in mode DelphiUnicode against those
units. Output goes to `FPC/bin`; the exit code is 0 when every test passed.

GitHub Actions runs the same build on every push and pull request to
`develop` (`.github/workflows/fpc-tests.yml`, in an Ubuntu 26.04 container, FPC
3.2.2, the same as the Linux development machine): it
downloads the Zeos 8.0.0 release above, checks its SHA-256 and runs
`FPC/build.sh`, so a change made in Delphi cannot break the Free Pascal build
unnoticed. The Delphi suite still runs on Windows only.

## Code Coverage

```powershell
.\run_coverage.ps1
```

Builds, runs all tests under
[DelphiCodeCoverage](https://github.com/DelphiCodeCoverage/DelphiCodeCoverage)
(expected at `C:\Attracs\DelphiCodeCoverage\`), and opens the HTML report
from `coverage_report/`. See the main [README](../README.md) for current
statistics and Codecov upload.
