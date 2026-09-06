# Bold for Delphi - Adapter x Engine test matrix
#
# Builds the test project once and runs it against each database engine in
# turn, then (unless skipped) builds the DebugUniDAC configuration and runs the
# UniDAC fixtures against SQL Server. The engine is passed through the
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
#   -UniDACFull  : Run the whole suite with the UniDAC build instead of only the
#                  UniDAC fixtures (Test.PersistenceUniDAC, Test.PersistenceScenariosUniDAC).
#   -UniDACDelphiVersion : Delphi registry version for the UniDAC build
#                  (default 23.0 = Delphi 12; UniDAC 10.4 does not compile on Delphi 13).
#
# Logs: UnitTest\matrix_<adapter>_<engine>.log. Exit code = number of failed runs.

param(
    [string[]]$Engines = @("SQLite", "SQLServer"),
    [string]$Filter = "",
    [switch]$SkipBuild,
    [switch]$SkipUniDAC,
    [switch]$UniDACFull,
    [string]$UniDACDelphiVersion = "23.0"
)

$ErrorActionPreference = "Stop"
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$BuildScript = Join-Path $ScriptDir "build.ps1"
$UniDACFixtures = "Test.PersistenceUniDAC,Test.PersistenceScenariosUniDAC"

$Results = New-Object System.Collections.Generic.List[object]
$SavedEngine = $env:BOLD_TEST_ENGINE

function Invoke-Build([string]$Config, [string]$DelphiVersion) {
    Write-Host ""
    Write-Host "[BUILD] $Config" -ForegroundColor Cyan
    # build.ps1 talks through Write-Host; capture every stream and show it only on failure.
    if ($DelphiVersion) {
        $BuildOutput = & $BuildScript -Config $Config -DelphiVersion $DelphiVersion *>&1 | ForEach-Object { "$_" }
    } else {
        $BuildOutput = & $BuildScript -Config $Config *>&1 | ForEach-Object { "$_" }
    }
    if ($LASTEXITCODE -ne 0) {
        $BuildOutput | Select-String -Pattern "error|fatal" | Select-Object -First 15 | ForEach-Object { Write-Host "  $_" -ForegroundColor Red }
        throw "Build of $Config failed (exit $LASTEXITCODE)"
    }
    Write-Host "  ok" -ForegroundColor Green
}

function Invoke-Run([string]$Adapter, [string]$Exe, [string]$Engine, [string]$RunFilter) {
    $Log = Join-Path $ScriptDir ("matrix_{0}_{1}.log" -f $Adapter, $Engine)
    Write-Host ""
    Write-Host ("[RUN] {0} x {1}" -f $Adapter, $Engine) -ForegroundColor Cyan
    $env:BOLD_TEST_ENGINE = $Engine
    $Args = @()
    if ($RunFilter) { $Args += "--run:$RunFilter" }
    $Started = Get-Date
    # DUnitX writes the FastMM leak report to stderr; keep both streams in the log.
    $Output = & $Exe @Args 2>&1 | ForEach-Object { "$_" }
    $Output | Set-Content -Path $Log -Encoding UTF8
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

    # ---- UniDAC build (Delphi 12), SQL Server only
    if (-not $SkipUniDAC) {
        if (-not $env:UniDAC) { $env:UniDAC = "C:\Attracs\Attracs-Common\components\UniDAC" }
        if (Test-Path (Join-Path $env:UniDAC "Source\Uni.pas")) {
            $UniDACExe = Join-Path $ScriptDir "UniDAC\UnitTest.exe"
            try {
                if (-not $SkipBuild) { Invoke-Build "DebugUniDAC" $UniDACDelphiVersion }
                if (-not (Test-Path $UniDACExe)) { throw "Not found: $UniDACExe" }
                $UniFilter = if ($Filter) { $Filter } elseif ($UniDACFull) { "" } else { $UniDACFixtures }
                Invoke-Run "UniDAC" $UniDACExe "SQLServer" $UniFilter
            } catch { Write-Host "  $_" -ForegroundColor Red }
        } else {
            Write-Host ""
            Write-Host "[SKIP] UniDAC not found at $env:UniDAC (UniDAC is commercial and optional)" -ForegroundColor Yellow
        }
    }
} finally {
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
