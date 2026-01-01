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
#
# EXAMPLES:
#   .\build.ps1
#   .\build.ps1 -Config Release -Platform Win64
#   .\build.ps1 -GUI

param(
    [string]$Config = "Debug",
    [string]$Platform = "Win32",
    [switch]$GUI
)

$ErrorActionPreference = "Stop"

# Set DUnitX environment variable (required for console test runner)
$env:DUnitX = "C:\Attracs\DUnitX\Source"

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
& $BuildScript -ProjectFile $ProjectPath -Config $Config -Platform $Platform

exit $LASTEXITCODE
