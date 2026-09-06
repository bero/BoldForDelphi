# Bold for Delphi - Unit Test Build Script
# Uses DelphiBuildDPROJ.ps1 for flexible Delphi version detection
#
# USAGE:
#   .\build.ps1 [-Config "Debug|Release"] [-Platform "Win32|Win64"] [-GUI]
#
# PARAMETERS:
#   -Config    : Build configuration (default: "Debug")
#   -Platform  : Target platform (default: "Win32")
#   -GUI       : Build UnitTestGUI.dproj instead of UnitTest.dproj
#   -DelphiVersion : Registry version of the Delphi to use (e.g. 23.0 = 12 Athens).
#                Default: newest installed. The vendored UniDAC 10.4 does not compile with
#                Delphi 13, so with that copy use -Config DebugUniDAC together with
#                -DelphiVersion 23.0; an installed UniDAC 11 for RAD Studio 13 needs no pin.
#   -Config DebugUniDAC : Debug + the real UniDAC test units (Code\Persistence\UniDAC)
#                instead of their empty stubs. Needs the UniDAC environment variable
#                (installation root with Source\ and/or Lib\Win32\); output goes to .\UniDAC\.
#
# EXAMPLES:
#   .\build.ps1
#   .\build.ps1 -Config Release -Platform Win64
#   .\build.ps1 -GUI

param(
    [string]$Config = "Debug",
    [string]$Platform = "Win32",
    [switch]$GUI,
    [string]$DelphiVersion = ""
)

$ErrorActionPreference = "Stop"

# DUnitX and Delphi-Mocks are required by the test projects.
# Respect variables that are already set (e.g. by setup-tests.ps1);
# fall back to the historical default location otherwise.
if (-not $env:DUnitX)      { $env:DUnitX      = "C:\Attracs\DUnitX\Source" }
if (-not $env:DelphiMocks) { $env:DelphiMocks = "C:\Attracs\Delphi-Mocks\Source" }

# UniDAC is a commercial component and is never part of this repository. Only the
# DebugUniDAC configuration compiles against it, through $(UniDAC)\Source; the
# default below is the copy vendored in the Attracs repositories.
if ($Config -eq "DebugUniDAC") {
    if (-not $env:UniDAC) { $env:UniDAC = "C:\Attracs\Attracs-Common\components\UniDAC" }
    # A source installation has Source\Uni.pas; the Devart installer alone ships compiled units in Lib\Win32.
    if (-not (Test-Path (Join-Path $env:UniDAC "Source\Uni.pas")) -and -not (Test-Path (Join-Path $env:UniDAC "Lib\Win32\Uni.dcu"))) {
        Write-Host "ERROR: UniDAC not found at $env:UniDAC (expected Source\Uni.pas or Lib\Win32\Uni.dcu)" -ForegroundColor Red
        exit 1
    }
}

if (-not (Test-Path $env:DUnitX) -or -not (Test-Path $env:DelphiMocks)) {
    Write-Host "ERROR: Unit test dependencies not found:" -ForegroundColor Red
    Write-Host "  DUnitX      = $env:DUnitX (exists: $(Test-Path $env:DUnitX))" -ForegroundColor Gray
    Write-Host "  DelphiMocks = $env:DelphiMocks (exists: $(Test-Path $env:DelphiMocks))" -ForegroundColor Gray
    Write-Host "Run .\setup-tests.ps1 to clone them and set the environment variables." -ForegroundColor Yellow
    exit 1
}

# Determine which project to build
if ($GUI) {
    $ProjectFile = "UnitTestGUI.dproj"
} else {
    $ProjectFile = "UnitTest.dproj"
}

# Path to the universal build script
$BuildScript = "C:\Attracs\DelphiStandards\DelphiBuildDPROJ.ps1"

if (-not (Test-Path $BuildScript)) {
    Write-Host "ERROR: Build script not found: $BuildScript" -ForegroundColor Red
    exit 1
}

# Get script directory for project path
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectPath = Join-Path $ScriptDir $ProjectFile

if (-not (Test-Path $ProjectPath)) {
    Write-Host "ERROR: Project file not found: $ProjectPath" -ForegroundColor Red
    exit 1
}

Write-Host "Building Bold for Delphi Unit Tests" -ForegroundColor Cyan
Write-Host "  Project: $ProjectFile" -ForegroundColor Gray
Write-Host "  Config:  $Config" -ForegroundColor Gray
Write-Host "  Platform: $Platform" -ForegroundColor Gray
Write-Host ""

# Call the universal build script
if ($DelphiVersion) {
    & $BuildScript -ProjectFile $ProjectPath -Config $Config -Platform $Platform -DelphiVersion $DelphiVersion
} else {
    & $BuildScript -ProjectFile $ProjectPath -Config $Config -Platform $Platform
}

exit $LASTEXITCODE
