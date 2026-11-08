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
| [FastMM4](https://github.com/pleriche/FastMM4) (optional) | `FastMM4` | Memory check `check_memory.ps1` only; MPL 1.1 / LGPL 2.1, not distributed with Bold |

The `.dproj` search paths reference them as `$(DUnitX)` and
`$(DelphiMocks)`. Each variable must point to the framework's **Source**
subfolder. `FastMM4` points to the FastMM4 folder itself, the one with
`FastMM4.pas`; nothing but `check_memory.ps1` needs it (see
[Memory leaks and freed memory](#memory-leaks-and-freed-memory)).

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
Run it after a change under `Source`, and after saving `dmInvoicing` in the IDE
designer: its `{%CLASSGROUP 'System.Classes.TPersistent'}` keeps the designer
from adding `BoldEnvironmentVCL` to its uses clause.

```powershell
.\check_no_vcl.ps1          # build Debug/Win64, check the map file
.\check_no_vcl.ps1 -Run     # ... and run the scenario against ..\Model\Invoicing.bld
```

## Cross-platform XML backend check

For Delphi, `BoldXMLStreaming` uses MSXML on Windows and Delphi's own
`Xml.xmldom` with the OmniXML vendor on other platforms (#117); Free Pascal uses
fcl-xml. To run the suite on the OmniXML backend without a macOS or Linux
target, add `BOLD_XMLDOM` to the conditional defines of a UnitTest build. Every
XML round trip in the suite then goes through it, and `Test.BoldXMLStreaming`
tests the backend directly. The Windows-only units that use MSXML themselves
(SOAP, XMI, COM) are not part of UnitTest and need MSXML.

## Delphi on Linux64

The console runner also builds for Linux64 (#117), with Delphi 12.3 or 13 and a
Linux SDK imported through the SDK Manager. The command-line build needs only
the imported SDK files; PAServer is needed to deploy and run from the IDE, not
to build:

```powershell
.\build.ps1 -Platform Linux64   # writes the ELF UnitTest (no extension) next to the script
```

Copy `UnitTest` and `UnitTest.ini` to a folder on the Linux machine and run it
there:

```sh
chmod +x UnitTest
./UnitTest --exit:Continue
```

FireDAC loads the unversioned `libsqlite3.so`, which most distributions ship
only in the development package (`sudo apt install libsqlite3-dev` on Ubuntu).
The results also go to `dunitx-results.xml` (NUnit format) in the current
folder.

The runner has one unit list for every platform. Test units whose subject is
Windows only (COM, WinInet, the Windows message loop of `BoldThread`, the VCL
control packs) are empty on other platforms, and so are the Delphi-Mocks
fixtures, because Delphi-Mocks does not compile for Linux64. The leak checks
read a counting wrapper around libc's `malloc` that `BoldTestFramework` installs
on Linux.

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

## Memory leaks and freed memory

Both suites can run with freed memory overwritten, so that a use of freed
memory fails instead of passing by luck, and with every block still allocated
at exit reported together with the line that allocated it.

### Delphi: FastMM4 FullDebugMode (`check_memory.ps1`)

```powershell
$env:FastMM4 = 'C:\path\to\FastMM4'                # a clone of https://github.com/pleriche/FastMM4
.\check_memory.ps1                                 # build DebugFastMM4/Win64, run the suite, check the log
.\check_memory.ps1 --run:Test.BoldXMLStreaming.    # a spot check: runner arguments pass through
.\check_memory.ps1 -FreedBlocks                    # check writes to freed blocks instead
```

`check_memory.ps1` builds `UnitTest.dproj` in the configuration `DebugFastMM4`
(exe, map file and event log in `FastMM4\`) and runs the suite under FastMM4 in
FullDebugMode. FastMM4 is the first unit of `UnitTest.dpr` under
`{$IFDEF BOLD_FASTMM4}`, which only that configuration defines, so normal builds
do not need FastMM4. An interface released after its object was freed, or a
virtual call on a freed object, is caught and logged with its stack trace, and
at exit FastMM4 logs every block still allocated with the stack trace of its
allocation, with unit and line from the detailed map file. The script prints
those entries and fails on any leak, any memory error and any failed test. The
full suite takes about a minute and a half. There is no Delphi CI, so run it in
every cycle, like `check_no_vcl.ps1`.

FastMM4 is optional and not distributed with Bold (MPL 1.1 / LGPL 2.1).
`FastMM4` points to the folder with `FastMM4.pas`, `FastMM4Options.inc` and
`FullDebugMode DLL\Precompiled`; the script copies the FullDebugMode DLL for the
platform next to the exe. It only reads that folder: it copies the units to
`FastMM4\src` and appends its settings to the copied `FastMM4Options.inc`
(FullDebugMode, everything logged to the event log, no message boxes), because
the original defines `RequireDebuggerPresenceForLeakReporting`, which a
command-line define cannot undo. One build checks either interfaces of freed
objects (the default) or writes to freed blocks (`-FreedBlocks`): FastMM4's
`CatchUseOfFreedInterfaces` turns the freed-block check off.

### Free Pascal: heaptrc

```sh
BOLD_HEAPTRC=1 sh FPC/build.sh                         # the suite under heaptrc
```

`BOLD_HEAPTRC=1` builds the runner with Free Pascal's heaptrc (`-gh`) and line
info (`-gl`) into `FPC/bin-heaptrc` and runs it. heaptrc fills freed memory with
`$F0`, so a use of freed memory raises instead of passing by luck, and at exit it
lists every block still allocated with the line that allocated it. `build.sh`
then prints that report and fails. CI runs the suite both ways.

### No per-test leak check

There is no per-test leak check: a cache created in one test and freed in a
later one makes per-test counts noisy. To pin a leak in a code path, a test
counts the heap around a loop, `BoldTestAllocatedBytes` under Free Pascal or
`BoldTestAllocatedBlocks` under Delphi, as `TestMalformedXMLDoesNotLeak` and
`TestFaultDoesNotLeak` do. The leak column of the Delphi runner always shows 0:
its default DUnitX leak monitor measures nothing.

## Code Coverage

```powershell
.\run_coverage.ps1
```

Builds, runs all tests under
[DelphiCodeCoverage](https://github.com/DelphiCodeCoverage/DelphiCodeCoverage)
(expected at `C:\Attracs\DelphiCodeCoverage\`), and opens the HTML report
from `coverage_report/`. See the main [README](../README.md) for current
statistics and Codecov upload.
