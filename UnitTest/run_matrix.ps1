# Bold for Delphi - Adapter x Engine test matrix
#
# Builds the test project once and runs it against each database engine in
# turn, then (unless skipped) builds the DebugUniDAC configuration and runs the
# UniDAC fixtures against each UniDAC engine. The engine is passed through the
# BOLD_TEST_ENGINE environment variable, which BoldTestDatabaseConfig reads
# before UnitTest.ini, so the ini is never edited.
#
# USAGE:
#   .\run_matrix.ps1 [-Engines SQLite,SQLServer,PostgreSQL] [-Filter <DUnitX --run filter>]
#                    [-SkipBuild] [-SkipUniDAC] [-UniDACFull] [-UniDACDelphiVersion 23.0]
#
# PARAMETERS:
#   -Engines     : Engines for the FireDAC build (default: SQLite, SQLServer, and
#                  PostgreSQL where it can run, see POSTGRESQL below). Each needs a
#                  section in UnitTest.ini. Through "powershell -File" a list such as
#                  SQLite,SQLServer arrives as one string, so entries are split on
#                  commas (-UniDACEngines too).
#   -Filter      : Optional DUnitX filter, e.g. Test.BoldBatchQueries or
#                  Test.BoldLinks.TTestBoldLinks.TestCollectRoleInPS_MatchesInMemory.
#   -SkipBuild   : Reuse the existing executables.
#   -SkipUniDAC  : Do not build or run the DebugUniDAC configuration.
#   -UniDACEngines : Engines for the UniDAC build. Default: the same list as -Engines when
#                  the UniDAC environment variable points at an installation with the
#                  SQLite provider (UniDAC 11), otherwise SQLServer only (the vendored
#                  10.4 copy has no SQLite provider). PostgreSQL is added when -Engines is
#                  left at its default, the installation has the PostgreSQL provider and
#                  the server answers.
#   -UniDACFull  : Run the whole suite with the UniDAC build instead of only the
#                  UniDAC fixtures (Test.PersistenceUniDAC, Test.PersistenceScenariosUniDAC,
#                  Test.BoldDbEvolutorUniDAC). On PostgreSQL the FireDAC tests in that exe
#                  need the libpq.dll of the FireDAC row; without one the PostgreSQL row
#                  keeps to the UniDAC fixtures.
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
# POSTGRESQL: the PostgreSQL rows need the server of [PostgreSQL] in UnitTest.ini.
# With -Engines left at its default, a row that cannot run is reported as skipped,
# not failed. FireDAC also needs a 64-bit libpq.dll (the test exes are Win64): the
# first one on the PATH, else the one in the bin folder of the newest PostgreSQL
# installation under Program Files. Its folder is put first on the PATH while a
# PostgreSQL row runs. UniDAC's provider talks to the server without it. The test
# bases create the databases when the server does not have them.
#
# Logs: UnitTest\matrix_<adapter>_<engine>.log. Exit code = number of failed runs
# (a skipped row is not one; a failed UniDAC build or a missing UniDAC exe is).
# Every run that was started leaves a row: a log that cannot be read or written
# (open in another program) costs only the log, and a run that could not be
# carried out is a FAIL row without counts.

# No positional parameters: a loose word (a filter without -Filter) is an
# error, not an engine
[CmdletBinding(PositionalBinding = $false)]
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
# "powershell -File ... -Engines SQLite,SQLServer" binds the list as one string
$Engines = @($Engines | ForEach-Object { $_ -split "," } | ForEach-Object { $_.Trim() } | Where-Object { $_ })
$UniDACEngines = @($UniDACEngines | ForEach-Object { $_ -split "," } | ForEach-Object { $_.Trim() } | Where-Object { $_ })
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$BuildScript = Join-Path $ScriptDir "build.ps1"
$UniDACFixtures = "Test.PersistenceUniDAC,Test.PersistenceScenariosUniDAC,Test.BoldDbEvolutorUniDAC"
$MatrixActivity = "Adapter x Engine matrix"

$Results = New-Object System.Collections.Generic.List[object]
$Skipped = New-Object System.Collections.Generic.List[string]
# Steps that leave no row in the table, a failed UniDAC build for one; each counts as a failed run
$FailedSteps = New-Object System.Collections.Generic.List[string]
$SavedEngine = $env:BOLD_TEST_ENGINE
$SavedPath = $env:PATH
# Set below for build.ps1 when unset. Left set, it would turn the default of
# -UniDACDelphiVersion into the newest Delphi for the next run in the same session.
$SavedUniDAC = $env:UniDAC

# ---- PostgreSQL helpers (see POSTGRESQL above)
# True when the file is a 64-bit (x64) PE image: the machine field of its header
function Test-Pe64([string]$Path) {
    try {
        $Bytes = [IO.File]::ReadAllBytes($Path)
        $PeHeader = [BitConverter]::ToInt32($Bytes, 0x3C)
        return [BitConverter]::ToUInt16($Bytes, $PeHeader + 4) -eq 0x8664
    } catch { return $false }
}

# The folder of a 64-bit libpq.dll: the first on the PATH, else the bin folder
# of the newest PostgreSQL installation under Program Files; "" when there is none
function Find-PostgreSQLBin {
    foreach ($Dir in ($env:PATH -split ";")) {
        if ($Dir -and (Test-Pe64 (Join-Path $Dir "libpq.dll"))) { return $Dir }
    }
    $ProgramFiles64 = if ($env:ProgramW6432) { $env:ProgramW6432 } else { $env:ProgramFiles }
    $Root = Join-Path $ProgramFiles64 "PostgreSQL"
    if (Test-Path -LiteralPath $Root) {
        $Newest = Get-ChildItem -LiteralPath $Root -Directory |
            Where-Object { ($_.Name -match '^\d+') -and (Test-Pe64 (Join-Path $_.FullName "bin\libpq.dll")) } |
            Sort-Object { [int]($_.Name -replace '^(\d+).*$', '$1') } -Descending | Select-Object -First 1
        if ($Newest) { return Join-Path $Newest.FullName "bin" }
    }
    return ""
}

# Server and port of [PostgreSQL] in UnitTest.ini, with the defaults of the test bases
function Get-PostgreSQLServer {
    $Server = "localhost"; $Port = 5432; $InSection = $false
    foreach ($Line in (Get-Content (Join-Path $ScriptDir "UnitTest.ini") -ErrorAction SilentlyContinue)) {
        if ($Line -match '^\s*\[(.+)\]') { $InSection = ($Matches[1] -eq "PostgreSQL"); continue }
        if ($InSection -and ($Line -match '^\s*Server\s*=\s*(.*?)\s*$')) { $Server = $Matches[1] }
        if ($InSection -and ($Line -match '^\s*Port\s*=\s*(\d+)')) { $Port = [int]$Matches[1] }
    }
    return @($Server, $Port)
}

function Test-TcpPort([string]$HostName, [int]$Port) {
    $Client = New-Object System.Net.Sockets.TcpClient
    try { return $Client.ConnectAsync($HostName, $Port).Wait(3000) -and $Client.Connected }
    catch { return $false }
    finally { $Client.Dispose() }
}

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
        $HasPostgreSQL = (Test-Path (Join-Path $env:UniDAC "Lib\Win32\PostgreSQLUniProvider.dcu")) -or
                         (Test-Path (Join-Path $env:UniDAC "Source\UniProviders\PostgreSQL\PostgreSQLUniProvider.pas"))
        if ($HasPostgreSQL -and -not $PSBoundParameters.ContainsKey("Engines")) { $UniDACEngines += "PostgreSQL" }
    }
}
$DefaultEngines = -not $PSBoundParameters.ContainsKey("Engines")
$PgBin = ""
if ($DefaultEngines -or ($Engines -contains "PostgreSQL") -or ($UniDACEngines -contains "PostgreSQL")) {
    $PgBin = Find-PostgreSQLBin
    $PgServer, $PgPort = Get-PostgreSQLServer
    $PgAnswers = Test-TcpPort $PgServer $PgPort
    # Rows the defaults add run only where they can; a row named in -Engines or
    # -UniDACEngines runs regardless and fails when it cannot
    if ($DefaultEngines) {
        $NoServer = "no server answers at ${PgServer}:$PgPort ([PostgreSQL] in UnitTest.ini)"
        if (-not $PgAnswers) { $Skipped.Add("FireDAC x PostgreSQL: $NoServer") }
        elseif (-not $PgBin) { $Skipped.Add("FireDAC x PostgreSQL: no 64-bit libpq.dll on the PATH or in a PostgreSQL\<version>\bin folder under Program Files") }
        else { $Engines += "PostgreSQL" }
        if (($UniDACEngines -contains "PostgreSQL") -and -not $PgAnswers -and -not $PSBoundParameters.ContainsKey("UniDACEngines")) {
            $UniDACEngines = @($UniDACEngines | Where-Object { $_ -ne "PostgreSQL" })
            $Skipped.Add("UniDAC x PostgreSQL: $NoServer")
        }
    }
}
$Skipped | ForEach-Object { Write-Host "[SKIP] $_" -ForegroundColor Yellow }
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
    # Only the progress bar needs it: a log that cannot be read (open in another
    # program) must not cost the run
    try {
        if ((Test-Path $Log) -and ((Get-Content -Path $Log -TotalCount 1) -eq $FilterLine)) {
            $Previous = Select-String -Path $Log -Pattern "^Tests Found\s*:\s*(\d+)" | Select-Object -First 1
            if ($Previous) { $Expected = [int]$Previous.Matches[0].Groups[1].Value }
        }
    } catch { $Expected = 0 }
    Start-Step "RUN" $Name
    $env:BOLD_TEST_ENGINE = $Engine
    # FireDAC loads libpq.dll from the PATH (see POSTGRESQL above)
    $env:PATH = if (($Engine -eq "PostgreSQL") -and $PgBin) { "$PgBin;$SavedPath" } else { $SavedPath }
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
    $env:PATH = $SavedPath
    Write-Progress -Id 2 -ParentId 1 -Activity $Name -Completed
    # The result is counted from $Output, so a log that cannot be written costs
    # the log, not the row
    $LogShown = $Log
    try { @($FilterLine) + @($Output) | Set-Content -Path $Log -Encoding UTF8 }
    catch {
        Write-Host "  log not written: $_" -ForegroundColor Yellow
        $LogShown = "$Log (not written)"
    }
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
        Seconds = [int]$Elapsed.TotalSeconds; Result = $(if ($Ok) { "OK" } else { "FAIL" }); Log = $LogShown
    })
    if (-not $Ok) {
        Write-Host ("  {0} failures/errors, leak={1}" -f ($Counts["Failed"] + $Counts["Errored"]), $Leak) -ForegroundColor Red
        $Failures | Select-Object -First 10 | ForEach-Object { Write-Host "  $_" -ForegroundColor Red }
    } else {
        Write-Host ("  {0} passed, {1} ignored, {2}s" -f $Counts["Passed"], $Counts["Ignored"], [int]$Elapsed.TotalSeconds) -ForegroundColor Green
    }
}

# A run that threw (the exe could not be started, for one) still gets its row,
# as FAIL without counts, unless Invoke-Run had added the row before it threw
function Add-FailedRun([string]$Adapter, [string]$Engine, $ErrorRecord) {
    Write-Host "  $ErrorRecord" -ForegroundColor Red
    if (@($Results | Where-Object { ($_.Adapter -eq $Adapter) -and ($_.Engine -eq $Engine) }).Count -eq 0) {
        $Results.Add([pscustomobject]@{
            Adapter = $Adapter; Engine = $Engine
            Found = -1; Passed = -1; Ignored = -1; Failed = -1; Errored = -1; Leak = $false
            Seconds = 0; Result = "FAIL"; Log = "(no log) $ErrorRecord"
        })
    }
}

try {
    # ---- FireDAC build, one run per engine
    $FireDACExe = Join-Path $ScriptDir "UnitTest.exe"
    if (-not $SkipBuild) { Invoke-Build "Debug" "" }
    if (-not (Test-Path $FireDACExe)) { throw "Not found: $FireDACExe" }
    foreach ($Engine in $Engines) {
        try { Invoke-Run "FireDAC" $FireDACExe $Engine $Filter }
        catch { Add-FailedRun "FireDAC" $Engine $_ }
    }

    # ---- UniDAC build, one run per UniDAC engine
    if ($UniDACAvailable) {
        try {
            if (-not $SkipBuild) { Invoke-Build "DebugUniDAC" $UniDACDelphiVersion }
            if (-not (Test-Path $UniDACExe)) { throw "Not found: $UniDACExe" }
            $UniFilter = if ($Filter) { $Filter } elseif ($UniDACFull) { "" } else { $UniDACFixtures }
            foreach ($Engine in $UniDACEngines) {
                # Without a 64-bit libpq the FireDAC tests in that exe cannot reach
                # PostgreSQL, so that row keeps to the UniDAC fixtures (see -UniDACFull)
                $EngineFilter = if (($Engine -eq "PostgreSQL") -and -not $Filter -and -not $PgBin) { $UniDACFixtures } else { $UniFilter }
                try { Invoke-Run "UniDAC" $UniDACExe $Engine $EngineFilter }
                catch { Add-FailedRun "UniDAC" $Engine $_ }
            }
        } catch {
            Write-Host "  $_" -ForegroundColor Red
            $FailedSteps.Add("UniDAC: $_")
        }
    } elseif (-not $SkipUniDAC) {
        Write-Host ""
        Write-Host "[SKIP] UniDAC not found at $env:UniDAC (UniDAC is commercial and optional)" -ForegroundColor Yellow
    }
} finally {
    Write-Progress -Id 1 -Activity $MatrixActivity -Completed
    if ($null -eq $SavedEngine) { Remove-Item Env:BOLD_TEST_ENGINE -ErrorAction SilentlyContinue }
    else { $env:BOLD_TEST_ENGINE = $SavedEngine }
    $env:PATH = $SavedPath
    if ($null -eq $SavedUniDAC) { Remove-Item Env:UniDAC -ErrorAction SilentlyContinue }
    else { $env:UniDAC = $SavedUniDAC }
}

Write-Host ""
Write-Host "Adapter x Engine matrix" -ForegroundColor Cyan
$Results | Format-Table Adapter, Engine, Found, Passed, Ignored, Failed, Errored, Leak, Seconds, Result -AutoSize | Out-String | Write-Host
$Results | ForEach-Object { Write-Host ("  log: {0}" -f $_.Log) -ForegroundColor Gray }
$Skipped | ForEach-Object { Write-Host ("  skipped: {0}" -f $_) -ForegroundColor Yellow }
$FailedSteps | ForEach-Object { Write-Host ("  failed: {0}" -f $_) -ForegroundColor Red }

$FailedRuns = @($Results | Where-Object { $_.Result -ne "OK" }).Count + $FailedSteps.Count
if ($Results.Count -eq 0) { $FailedRuns = [Math]::Max($FailedRuns, 1) }
exit $FailedRuns
