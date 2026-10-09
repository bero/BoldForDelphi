# Bold for Delphi - Adapter x Engine test matrix
#
# Builds the test project once and runs it against each database engine in
# turn, then (unless skipped) builds the DebugUniDAC configuration and runs the
# UniDAC fixtures against each UniDAC engine. The engine is passed through the
# BOLD_TEST_ENGINE environment variable, which BoldTestDatabaseConfig reads
# before UnitTest.ini, so the ini is never edited.
#
# USAGE:
#   .\run_matrix.ps1 [-Engines SQLite,SQLServer] [-Filter <DUnitX --run filter>]
#                    [-SkipBuild] [-SkipUniDAC] [-UniDACFull] [-UniDACDelphiVersion 23.0]
#
# PARAMETERS:
#   -Engines     : Engines for the FireDAC build (default: SQLite, SQLServer).
#                  Each needs a section in UnitTest.ini.
#   -Filter      : Optional DUnitX filter, e.g. Test.BoldBatchQueries or
#                  Test.BoldLinks.TTestBoldLinks.TestCollectRoleInPS_MatchesInMemory.
#   -SkipBuild   : Reuse the existing executables.
#   -SkipUniDAC  : Do not build or run the DebugUniDAC configuration.
#   -UniDACEngines : Engines for the UniDAC build. Default: the same list as -Engines when
#                  the UniDAC environment variable points at an installation with the
#                  SQLite provider (UniDAC 11), otherwise SQLServer only (the vendored
#                  10.4 copy has no SQLite provider).
#   -UniDACFull  : Run the whole suite with the UniDAC build instead of only the
#                  UniDAC fixtures (Test.PersistenceUniDAC, Test.PersistenceScenariosUniDAC).
#   -UniDACDelphiVersion : Delphi registry version for the UniDAC build. Default: 23.0
#                  (Delphi 12) when the vendored Attracs-Common copy is used, because stock
#                  UniDAC 10.4 does not compile on Delphi 13; the newest Delphi when the
#                  UniDAC environment variable points at another installation (for example
#                  the patched copy in C:\Attracs\UniDAC-10.4-D13, see its PATCHES.md).
#
# PROGRESS: every step prints [k/N]. In a console window two progress bars show
# the current step and, during a build, the elapsed time or, during a run, the
# tests done so far - out of the count in the previous log of the same row when
# that log was written for the same filter (the first line of each log).
#
# Logs: UnitTest\matrix_<adapter>_<engine>.log. Exit code = number of failed runs.

param(
    [string[]]$Engines = @("SQLite", "SQLServer"),
    [string]$Filter = "",
    [switch]$SkipBuild,
    [switch]$SkipUniDAC,
    [switch]$UniDACFull,
    [string[]]$UniDACEngines = @(),
    [string]$UniDACDelphiVersion = $(if ($env:UniDAC) { "" } else { "23.0" })
)

$ErrorActionPreference = "Stop"
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$BuildScript = Join-Path $ScriptDir "build.ps1"
$UniDACFixtures = "Test.PersistenceUniDAC,Test.PersistenceScenariosUniDAC"
$MatrixActivity = "Adapter x Engine matrix"

$Results = New-Object System.Collections.Generic.List[object]
$SavedEngine = $env:BOLD_TEST_ENGINE

# ---- Plan the steps up front, so every header can show [k/N]
$UniDACExe = Join-Path $ScriptDir "UniDAC\UnitTest.exe"
$UniDACAvailable = $false
if (-not $SkipUniDAC) {
    if (-not $env:UniDAC) { $env:UniDAC = "C:\Attracs\Attracs-Common\components\UniDAC" }
    # source installation (Source\Uni.pas) or the Devart installer's compiled units (Lib\Win32\Uni.dcu)
    $UniDACAvailable = (Test-Path (Join-Path $env:UniDAC "Source\Uni.pas")) -or (Test-Path (Join-Path $env:UniDAC "Lib\Win32\Uni.dcu"))
    if ($UniDACAvailable -and $UniDACEngines.Count -eq 0) {
        $HasSQLite = (Test-Path (Join-Path $env:UniDAC "Lib\Win32\SQLiteUniProvider.dcu")) -or
                     (Test-Path (Join-Path $env:UniDAC "Source\UniProviders\SQLite\SQLiteUniProvider.pas"))
        $UniDACEngines = if ($HasSQLite) { $Engines } else { @("SQLServer") }
    }
}
$TotalSteps = $Engines.Count
if (-not $SkipBuild) { $TotalSteps++ }
if ($UniDACAvailable) {
    $TotalSteps += $UniDACEngines.Count
    if (-not $SkipBuild) { $TotalSteps++ }
}
$Step = 0

function Start-Step([string]$Kind, [string]$Name) {
    $script:Step++
    Write-Host ""
    Write-Host ("[{0}/{1}] {2} {3}" -f $script:Step, $TotalSteps, $Kind, $Name) -ForegroundColor Cyan
    Write-Progress -Id 1 -Activity $MatrixActivity -Status ("Step {0}/{1}: {2} {3}" -f $script:Step, $TotalSteps, $Kind, $Name) `
        -PercentComplete ([int](100 * ($script:Step - 1) / $TotalSteps))
}

function Invoke-Build([string]$Config, [string]$DelphiVersion) {
    Start-Step "BUILD" $Config
    $Activity = "Building $Config"
    $Started = [Diagnostics.Stopwatch]::StartNew()
    $Shown = [Diagnostics.Stopwatch]::StartNew()
    $BuildArgs = @{ Config = $Config }
    if ($DelphiVersion) { $BuildArgs.DelphiVersion = $DelphiVersion }
    # build.ps1 talks through Write-Host; capture every stream and show it only on failure.
    # MSBuild reports no progress of its own, so the bar shows the elapsed time.
    $BuildOutput = & $BuildScript @BuildArgs *>&1 | ForEach-Object {
        if ($Shown.ElapsedMilliseconds -ge 500) {
            Write-Progress -Id 2 -ParentId 1 -Activity $Activity -Status ("{0} s" -f [int]$Started.Elapsed.TotalSeconds)
            $Shown.Restart()
        }
        "$_"
    }
    Write-Progress -Id 2 -ParentId 1 -Activity $Activity -Completed
    if ($LASTEXITCODE -ne 0) {
        $BuildOutput | Select-String -Pattern "error|fatal" | Select-Object -First 15 | ForEach-Object { Write-Host "  $_" -ForegroundColor Red }
        throw "Build of $Config failed (exit $LASTEXITCODE)"
    }
    Write-Host ("  ok, {0}s" -f [int]$Started.Elapsed.TotalSeconds) -ForegroundColor Green
}

function Show-RunProgress([string]$Name, [int]$Done, [int]$Expected) {
    if ($Expected -gt 0) {
        Write-Progress -Id 2 -ParentId 1 -Activity $Name -Status ("{0}/{1} tests" -f $Done, $Expected) `
            -PercentComplete ([Math]::Min(100, [int](100 * $Done / $Expected)))
    } else {
        Write-Progress -Id 2 -ParentId 1 -Activity $Name -Status ("{0} tests" -f $Done)
    }
}

function Invoke-Run([string]$Adapter, [string]$Exe, [string]$Engine, [string]$RunFilter) {
    $Name = "{0} x {1}" -f $Adapter, $Engine
    $Log = Join-Path $ScriptDir ("matrix_{0}_{1}.log" -f $Adapter, $Engine)
    # The previous log of the same row gives the expected number of tests, as
    # long as it was written for the same filter (its first line).
    $FilterLine = "Filter: " + $(if ($RunFilter) { $RunFilter } else { "(none)" })
    $Expected = 0
    if ((Test-Path $Log) -and ((Get-Content -Path $Log -TotalCount 1) -eq $FilterLine)) {
        $Previous = Select-String -Path $Log -Pattern "^Tests Found\s*:\s*(\d+)" | Select-Object -First 1
        if ($Previous) { $Expected = [int]$Previous.Matches[0].Groups[1].Value }
    }
    Start-Step "RUN" $Name
    $env:BOLD_TEST_ENGINE = $Engine
    $Args = @()
    if ($RunFilter) { $Args += "--run:$RunFilter" }
    $Started = Get-Date
    $Done = 0
    $Shown = [Diagnostics.Stopwatch]::StartNew()
    # DUnitX writes the FastMM leak report to stderr; keep both streams in the log.
    # Under "Stop" the first stderr line would be a terminating error and the run
    # would vanish from the table, so the exe runs under "Continue".
    $ErrorActionPreference = "Continue"
    $Output = & $Exe @Args 2>&1 | ForEach-Object {
        $Line = "$_"
        if ($Line -match "Executing Test :") {
            $Done++
            if ($Shown.ElapsedMilliseconds -ge 250) {
                Show-RunProgress $Name $Done $Expected
                $Shown.Restart()
            }
        }
        $Line
    }
    $ErrorActionPreference = "Stop"
    Write-Progress -Id 2 -ParentId 1 -Activity $Name -Completed
    @($FilterLine) + @($Output) | Set-Content -Path $Log -Encoding UTF8
    $Elapsed = (Get-Date) - $Started

    $Counts = @{}
    foreach ($Key in "Found", "Ignored", "Passed", "Failed", "Errored") {
        $Line = $Output | Select-String -Pattern ("^Tests {0}\s*:\s*(\d+)" -f $Key) | Select-Object -First 1
        $Counts[$Key] = if ($Line) { [int]$Line.Matches[0].Groups[1].Value } else { -1 }
    }
    $Leak = [bool]($Output | Select-String -Pattern "Unexpected Memory Leak" -Quiet)
    $Failures = $Output | Select-String -Pattern "^\s*Message:" | ForEach-Object { $_.Line.Trim() }

    $Ok = ($Counts["Found"] -gt 0) -and ($Counts["Failed"] -eq 0) -and ($Counts["Errored"] -eq 0) -and -not $Leak
    $Results.Add([pscustomobject]@{
        Adapter = $Adapter; Engine = $Engine
        Found = $Counts["Found"]; Passed = $Counts["Passed"]; Ignored = $Counts["Ignored"]
        Failed = $Counts["Failed"]; Errored = $Counts["Errored"]; Leak = $Leak
        Seconds = [int]$Elapsed.TotalSeconds; Result = $(if ($Ok) { "OK" } else { "FAIL" }); Log = $Log
    })
    if (-not $Ok) {
        Write-Host ("  {0} failures/errors, leak={1}" -f ($Counts["Failed"] + $Counts["Errored"]), $Leak) -ForegroundColor Red
        $Failures | Select-Object -First 10 | ForEach-Object { Write-Host "  $_" -ForegroundColor Red }
    } else {
        Write-Host ("  {0} passed, {1} ignored, {2}s" -f $Counts["Passed"], $Counts["Ignored"], [int]$Elapsed.TotalSeconds) -ForegroundColor Green
    }
}

try {
    # ---- FireDAC build, one run per engine
    $FireDACExe = Join-Path $ScriptDir "UnitTest.exe"
    if (-not $SkipBuild) { Invoke-Build "Debug" "" }
    if (-not (Test-Path $FireDACExe)) { throw "Not found: $FireDACExe" }
    foreach ($Engine in $Engines) {
        try { Invoke-Run "FireDAC" $FireDACExe $Engine $Filter }
        catch { Write-Host "  $_" -ForegroundColor Red }
    }

    # ---- UniDAC build, one run per UniDAC engine
    if ($UniDACAvailable) {
        try {
            if (-not $SkipBuild) { Invoke-Build "DebugUniDAC" $UniDACDelphiVersion }
            if (-not (Test-Path $UniDACExe)) { throw "Not found: $UniDACExe" }
            $UniFilter = if ($Filter) { $Filter } elseif ($UniDACFull) { "" } else { $UniDACFixtures }
            foreach ($Engine in $UniDACEngines) {
                try { Invoke-Run "UniDAC" $UniDACExe $Engine $UniFilter }
                catch { Write-Host "  $_" -ForegroundColor Red }
            }
        } catch { Write-Host "  $_" -ForegroundColor Red }
    } elseif (-not $SkipUniDAC) {
        Write-Host ""
        Write-Host "[SKIP] UniDAC not found at $env:UniDAC (UniDAC is commercial and optional)" -ForegroundColor Yellow
    }
} finally {
    Write-Progress -Id 1 -Activity $MatrixActivity -Completed
    if ($null -eq $SavedEngine) { Remove-Item Env:BOLD_TEST_ENGINE -ErrorAction SilentlyContinue }
    else { $env:BOLD_TEST_ENGINE = $SavedEngine }
}

Write-Host ""
Write-Host "Adapter x Engine matrix" -ForegroundColor Cyan
$Results | Format-Table Adapter, Engine, Found, Passed, Ignored, Failed, Errored, Leak, Seconds, Result -AutoSize | Out-String | Write-Host
$Results | ForEach-Object { Write-Host ("  log: {0}" -f $_.Log) -ForegroundColor Gray }

$FailedRuns = @($Results | Where-Object { $_.Result -ne "OK" }).Count
if ($Results.Count -eq 0) { $FailedRuns = 1 }
exit $FailedRuns
