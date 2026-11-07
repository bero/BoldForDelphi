# Bold for Delphi - memory check of the Delphi test suite with FastMM4 FullDebugMode (#117)
#
# Builds UnitTest.dproj in the configuration DebugFastMM4 and runs the suite
# under FastMM4 in FullDebugMode. Freed memory is overwritten there, so a use of
# freed memory (an interface released after its object, a virtual call on a
# freed object) fails instead of passing by luck, and at exit every block still
# allocated is logged with the stack trace of its allocation. The script then
# reads FastMM4's event log and fails on any leak or memory error, printing the
# entries with their stack traces. It is the Delphi counterpart of the Free
# Pascal heaptrc run (BOLD_HEAPTRC=1 sh FPC/build.sh). There is no Delphi CI, so
# run it in every cycle, like check_no_vcl.ps1. The leak column of the DUnitX
# runner stays meaningless: its default leak monitor measures nothing.
#
# SETUP: FastMM4 is optional and not distributed with Bold (MPL 1.1 / LGPL 2.1).
# Clone https://github.com/pleriche/FastMM4 and set the environment variable
# FastMM4 to that folder. The folder is only read: the script copies the units
# to FastMM4\src and appends its settings to the copied FastMM4Options.inc,
# because that file defines RequireDebuggerPresenceForLeakReporting
# unconditionally, which a command-line define cannot undo. The FullDebugMode
# DLL for the platform is copied next to the exe, from "FullDebugMode
# DLL\Precompiled" in the FastMM4 folder.
#
# USAGE:
#   .\check_memory.ps1 [-Platform Win64] [-DelphiVersion 37.0] [-SkipBuild] [-FreedBlocks]
#                      [-TimeoutMinutes 30] [runner arguments, e.g. --run:Test.BoldXMLStreaming.]
#
# PARAMETERS:
#   -Platform       : Win32 or Win64 (default: Win64).
#   -DelphiVersion  : Registry version of the Delphi to use (e.g. 37.0 = 13). Default: newest.
#   -SkipBuild      : Run the previous DebugFastMM4 build again.
#   -FreedBlocks    : Check writes to freed blocks instead of uses of interfaces of freed
#                     objects. FastMM4 cannot do both in one build: CatchUseOfFreedInterfaces
#                     turns off the freed-block check. The default is the interface check,
#                     because releasing an interface after its object was freed is the
#                     mistake the suite has actually made.
#   -TimeoutMinutes : Kill the run after this many minutes (default: 30).
#   Anything else is passed to the runner, after --consolemode:Quiet --exit:Continue.
#
# EXIT CODE: 0 when every test passes and the event log has no leak and no error;
# 1 otherwise.

param(
    [string]$Platform = "Win64",
    [string]$DelphiVersion = "",
    [switch]$SkipBuild,
    [switch]$FreedBlocks,
    [int]$TimeoutMinutes = 30,
    [Parameter(ValueFromRemainingArguments = $true)]
    [string[]]$RunnerArgs = @()
)

$BuildScript = "C:\Attracs\DelphiStandards\DelphiBuildDPROJ.ps1"
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RepoRoot = Split-Path -Parent $ScriptDir
$ProjectPath = Join-Path $ScriptDir "UnitTest.dproj"
$OutDir = Join-Path $ScriptDir "FastMM4"
$SrcDir = Join-Path $OutDir "src"
$ExeFile = Join-Path $OutDir "UnitTest.exe"
$MapFile = Join-Path $OutDir "UnitTest.map"
$LogFile = Join-Path $OutDir "UnitTest_MemoryManager_EventLog.txt"
$StdOutFile = Join-Path $OutDir "UnitTest_output.txt"
$DllName = if ($Platform -eq "Win32") { "FastMM_FullDebugMode.dll" } else { "FastMM_FullDebugMode64.dll" }

if (-not $env:DUnitX)      { $env:DUnitX      = "C:\Attracs\DUnitX\Source" }
if (-not $env:DelphiMocks) { $env:DelphiMocks = "C:\Attracs\Delphi-Mocks\Source" }

if (-not $SkipBuild) {
    if (-not $env:FastMM4 -or -not (Test-Path (Join-Path $env:FastMM4 "FastMM4.pas"))) {
        Write-Host "ERROR: set the environment variable FastMM4 to a clone of https://github.com/pleriche/FastMM4" -ForegroundColor Red
        exit 1
    }
    $Dll = Join-Path $env:FastMM4 "FullDebugMode DLL\Precompiled\$DllName"
    if (-not (Test-Path $Dll)) {
        Write-Host "ERROR: $Dll not found" -ForegroundColor Red
        exit 1
    }
    if (-not (Test-Path $BuildScript)) {
        Write-Host "ERROR: Build script not found: $BuildScript" -ForegroundColor Red
        exit 1
    }

    # Stage the FastMM4 units and append the settings of this check to the copied options.
    New-Item -ItemType Directory -Force $SrcDir | Out-Null
    Get-ChildItem $SrcDir -File | Remove-Item -Force
    Copy-Item (Join-Path $env:FastMM4 "FastMM4*.pas") $SrcDir
    $Options = [System.IO.File]::ReadAllText((Join-Path $env:FastMM4 "FastMM4Options.inc"))
    $Interfaces = if ($FreedBlocks) { "{`$undef CatchUseOfFreedInterfaces}" } else { "{`$define CatchUseOfFreedInterfaces}" }
    $Options += @"


{ Appended by Bold's check_memory.ps1, in this staged copy only: an unattended
  FullDebugMode run that logs every leak and error to the event log. }
{`$define FullDebugMode}
{`$define RawStackTraces}
$Interfaces
{`$define LogErrorsToFile}
{`$define LogMemoryLeakDetailToFile}
{`$define ClearLogFileOnStartup}
{`$define EnableMemoryLeakReporting}
{`$undef RequireDebuggerPresenceForLeakReporting}
{`$undef RequireIDEPresenceForLeakReporting}
{`$undef RequireDebugInfoForLeakReporting}
{`$undef ManualLeakReportingControl}
{`$undef LoadDebugDLLDynamically}
{`$define NoMessageBoxes}
"@
    [System.IO.File]::WriteAllText((Join-Path $SrcDir "FastMM4Options.inc"), ($Options -replace "(?<!\r)\n", "`r`n"))

    $Mode = if ($FreedBlocks) { "freed-block check" } else { "freed-interface check" }
    Write-Host "Building UnitTest (DebugFastMM4, $Platform, FastMM4 FullDebugMode, $Mode)" -ForegroundColor Cyan
    Remove-Item $ExeFile, $MapFile -ErrorAction SilentlyContinue
    if ($DelphiVersion) {
        & $BuildScript -ProjectFile $ProjectPath -Config DebugFastMM4 -Platform $Platform -DelphiVersion $DelphiVersion
    } else {
        & $BuildScript -ProjectFile $ProjectPath -Config DebugFastMM4 -Platform $Platform
    }
    if ($LASTEXITCODE -ne 0 -or -not (Test-Path $ExeFile)) {
        Write-Host "ERROR: UnitTest build failed (exit $LASTEXITCODE)" -ForegroundColor Red
        exit 1
    }
    Copy-Item $Dll $OutDir -Force
}

if (-not (Test-Path $ExeFile)) {
    Write-Host "ERROR: $ExeFile not found (was the build skipped?)" -ForegroundColor Red
    exit 1
}
if (-not (Test-Path $MapFile)) {
    Write-Host "WARNING: $MapFile not found; stack traces will show addresses only" -ForegroundColor Yellow
}

# Run the suite unattended. FastMM4 shows no message box (NoMessageBoxes), so TestRunner's
# ReportMemoryLeaksOnShutdown := True cannot block the run; the timeout guards anything else.
Remove-Item $LogFile, $StdOutFile -ErrorAction SilentlyContinue
$AllArgs = @("--consolemode:Quiet", "--exit:Continue") + $RunnerArgs
Write-Host ("Running: UnitTest.exe {0}" -f ($AllArgs -join ' ')) -ForegroundColor Cyan
$Watch = [System.Diagnostics.Stopwatch]::StartNew()
$Process = Start-Process -FilePath $ExeFile -ArgumentList $AllArgs -WorkingDirectory $RepoRoot `
    -RedirectStandardOutput $StdOutFile -RedirectStandardError "$StdOutFile.err" -PassThru -NoNewWindow
$null = $Process.Handle
if (-not $Process.WaitForExit($TimeoutMinutes * 60000)) {
    $Process.Kill()
    Write-Host "ERROR: the run did not finish within $TimeoutMinutes minutes and was killed" -ForegroundColor Red
    exit 1
}
$Process.WaitForExit()
$RunExit = $Process.ExitCode
$Seconds = [int]$Watch.Elapsed.TotalSeconds

$Output = @(Get-Content $StdOutFile -ErrorAction SilentlyContinue)
$Summary = @($Output | Where-Object { $_ -match '^Tests (Found|Passed|Failed|Errored)\s*:' } | ForEach-Object { $_.Trim() })
$Failing = @($Output | Where-Object { $_ -match '^\s+(Test\.\S+|Message: .*)$' } | ForEach-Object { $_.Trim() })

# The event log holds one entry per leak or error, each preceded by a line of dashes with a time stamp.
$Leaks = @()
$Errors = @()
if (Test-Path $LogFile) {
    $Entries = [regex]::Split([System.IO.File]::ReadAllText($LogFile), "(?m)^-{10,}.*-{10,}\r?$") |
        ForEach-Object { $_.Trim() } | Where-Object { $_ }
    foreach ($Entry in $Entries) {
        if ($Entry -match '^This application has leaked memory') { continue }  # the summary of the leak entries
        if ($Entry -match '^A memory block has been leaked') { $Leaks += $Entry } else { $Errors += $Entry }
    }
}

Write-Host ("Suite: {0} in {1} s (runner exit {2})" -f ($Summary -join ' | '), $Seconds, $RunExit)
Write-Host ("Event log: {0} leak(s), {1} error(s)" -f $Leaks.Count, $Errors.Count)
foreach ($Entry in ($Errors + $Leaks)) {
    Write-Host "----" -ForegroundColor Red
    Write-Host ($Entry -replace '(?s)\s*Current memory dump of.*$', '')  # the dump stays in the log
}
if ($Failing.Count -gt 0) {
    Write-Host "Failing tests:" -ForegroundColor Red
    $Failing | ForEach-Object { Write-Host "  $_" -ForegroundColor Red }
}

if ($RunExit -ne 0 -or $Leaks.Count -gt 0 -or $Errors.Count -gt 0 -or $Summary.Count -eq 0) {
    Write-Host "ERROR: memory check failed (log: $LogFile)" -ForegroundColor Red
    exit 1
}
Write-Host "OK: all tests passed, no leak and no memory error" -ForegroundColor Green
exit 0
