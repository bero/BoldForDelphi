# Builds the DUnitX tests of the standalone model editor (#128) and runs them.
#
# USAGE:
#   .\build.ps1 [-Platform Win64] [-Config Debug] [-DelphiVersion 37.0] [-NoRun]
#
# DUnitX is taken from $env:DUnitX, as for UnitTest\build.ps1
# (default C:\Attracs\DUnitX\Source). The exit code is the build's, or the
# test run's: non-zero when a test fails.

param(
    [string]$Platform = "Win64",
    [string]$Config = "Debug",
    [string]$DelphiVersion = "",
    [switch]$NoRun
)

$BuildScript = "C:\Attracs\DelphiStandards\DelphiBuildDPROJ.ps1"
$Here = Split-Path -Parent $MyInvocation.MyCommand.Path
$Project = Join-Path $Here "ModelEditorTests.dproj"

if (-not $env:DUnitX) { $env:DUnitX = "C:\Attracs\DUnitX\Source" }
if (-not (Test-Path $env:DUnitX)) {
    Write-Host "ERROR: DUnitX not found at $env:DUnitX; set `$env:DUnitX" -ForegroundColor Red
    exit 1
}

if ($DelphiVersion) {
    & $BuildScript -ProjectFile $Project -Config $Config -Platform $Platform -DelphiVersion $DelphiVersion
} else {
    & $BuildScript -ProjectFile $Project -Config $Config -Platform $Platform
}
if ($LASTEXITCODE -ne 0) {
    Write-Host "ERROR: ModelEditorTests build failed (exit $LASTEXITCODE)" -ForegroundColor Red
    exit $LASTEXITCODE
}
if ($NoRun) { exit 0 }

& (Join-Path $Here "ModelEditorTests.exe") --exit:Continue
exit $LASTEXITCODE
