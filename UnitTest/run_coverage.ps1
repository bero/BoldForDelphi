# Bold for Delphi - Code Coverage Script
# Uses DelphiBuildDPROJ.ps1 for flexible Delphi version detection
#
# USAGE:
#   .\run_coverage.ps1 [-SkipBuild] [-OpenReport]
#
# PARAMETERS:
#   -SkipBuild   : Skip build, only run coverage analysis on existing executable
#   -OpenReport  : Open the coverage report in browser after completion
#
# EXAMPLES:
#   .\run_coverage.ps1
#   .\run_coverage.ps1 -SkipBuild
#   .\run_coverage.ps1 -OpenReport

param(
    [switch]$SkipBuild,
    [switch]$OpenReport
)

$ErrorActionPreference = "Stop"

# Set DUnitX environment variable (required for console test runner)
$env:DUnitX = "C:\Attracs\DUnitX\Source"

# Configuration
$CoverageExe = "C:\Attracs\DelphiCodeCoverage\build\Win32\CodeCoverage.exe"
$ProjectName = "UnitTest"
$Executable = "$ProjectName.exe"
$MapFile = "$ProjectName.map"
$SourceDir = "..\Source"
$OutputDir = "coverage_report"
$BuildScript = "C:\Attracs\DelphiStandards\DelphiBuildDPROJ.ps1"

# Get script directory
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
Push-Location $ScriptDir

try {
    Write-Host "Bold for Delphi - Code Coverage" -ForegroundColor Cyan
    Write-Host "=" * 50 -ForegroundColor DarkGray
    Write-Host ""

    # Check for CodeCoverage tool
    if (-not (Test-Path $CoverageExe)) {
        Write-Host "ERROR: CodeCoverage.exe not found at: $CoverageExe" -ForegroundColor Red
        Write-Host "Please download from: https://github.com/DelphiCodeCoverage/DelphiCodeCoverage/releases" -ForegroundColor Yellow
        exit 1
    }

    # Build if not skipping
    if (-not $SkipBuild) {
        Write-Host "[BUILD] Building $ProjectName with MAP file..." -ForegroundColor Yellow
        Write-Host ""

        if (-not (Test-Path $BuildScript)) {
            Write-Host "ERROR: Build script not found: $BuildScript" -ForegroundColor Red
            exit 1
        }

        $ProjectPath = Join-Path $ScriptDir "$ProjectName.dproj"

        # We need to build with MAP file generation enabled
        # The DelphiBuildDPROJ.ps1 script uses MSBuild, so we need to call it
        # and add the DCC_MapFile property

        # Initialize Delphi environment and build with MAP file
        Write-Host "Initializing Delphi environment..." -ForegroundColor Gray

        # Source the DelphiBuildDPROJ functions by dot-sourcing or call directly
        # For simplicity, we'll invoke it and handle MAP file separately

        # First, let's find rsvars.bat using registry (similar to DelphiBuildDPROJ.ps1)
        $RegistryPaths = @(
            "HKLM:\SOFTWARE\Embarcadero\BDS",
            "HKLM:\SOFTWARE\WOW6432Node\Embarcadero\BDS"
        )

        $DelphiPath = $null
        foreach ($RegPath in $RegistryPaths) {
            if (Test-Path $RegPath) {
                $BDSKeys = Get-ChildItem -Path $RegPath -ErrorAction SilentlyContinue
                foreach ($Key in $BDSKeys) {
                    $VersionName = $Key.PSChildName
                    if ($VersionName -match '^\d+\.\d+$') {
                        $RootDir = Get-ItemProperty -Path $Key.PSPath -Name "RootDir" -ErrorAction SilentlyContinue
                        if ($RootDir -and $RootDir.RootDir -and (Test-Path $RootDir.RootDir)) {
                            $DelphiPath = $RootDir.RootDir
                        }
                    }
                }
            }
        }

        if (-not $DelphiPath) {
            Write-Host "ERROR: Could not find Delphi installation" -ForegroundColor Red
            exit 1
        }

        $RSVars = Join-Path $DelphiPath "bin\rsvars.bat"
        Write-Host "  Using Delphi: $DelphiPath" -ForegroundColor Gray

        # Execute rsvars.bat and capture environment
        $tempFile = [System.IO.Path]::GetTempFileName()
        cmd /c "`"$RSVars`" && set > `"$tempFile`""
        Get-Content $tempFile | ForEach-Object {
            if ($_ -match "^(.*?)=(.*)$") {
                Set-Item -Path "env:$($matches[1])" -Value $matches[2]
            }
        }
        Remove-Item $tempFile

        # Build with MAP file generation (DCC_MapFile=3 for detailed MAP)
        $MSBuildArgs = @(
            "$ProjectName.dproj",
            "/t:Build",
            "/p:Config=Debug",
            "/p:Platform=Win32",
            "/p:DCC_MapFile=3",
            "/p:DCC_Define=DEBUG",
            "/v:minimal",
            "/nologo"
        )

        Write-Host "  Running MSBuild..." -ForegroundColor Gray
        $BuildResult = & msbuild @MSBuildArgs
        $BuildExitCode = $LASTEXITCODE

        if ($BuildExitCode -ne 0) {
            Write-Host ""
            Write-Host "ERROR: Build failed with exit code $BuildExitCode" -ForegroundColor Red
            exit 1
        }

        # Verify MAP file was generated
        if (-not (Test-Path $MapFile)) {
            Write-Host ""
            Write-Host "ERROR: Build succeeded but $MapFile is missing" -ForegroundColor Red
            Write-Host "       Check output paths in Project Options" -ForegroundColor Yellow
            exit 1
        }

        Write-Host ""
        Write-Host "[BUILD] Build completed successfully" -ForegroundColor Green
        Write-Host ""
    }

    # Verify executable and MAP file exist
    if (-not (Test-Path $Executable)) {
        Write-Host "ERROR: $Executable not found. Build the project first." -ForegroundColor Red
        exit 1
    }
    if (-not (Test-Path $MapFile)) {
        Write-Host "ERROR: $MapFile not found. Build with MAP file generation enabled." -ForegroundColor Red
        exit 1
    }

    # Create output directory
    if (-not (Test-Path $OutputDir)) {
        New-Item -ItemType Directory -Path $OutputDir | Out-Null
    }

    # Run coverage analysis
    Write-Host "[COVERAGE] Running code coverage analysis..." -ForegroundColor Yellow
    Write-Host ""

    $CoverageArgs = @(
        "-e", $Executable,
        "-m", $MapFile,
        "-sd", $SourceDir,
        "-lcl", "3",
        "-spf", "coverage_source_paths.lst",
        "-uf", "coverage_units.lst",
        "-html",
        "-xml",
        "-od", $OutputDir
    )

    & $CoverageExe @CoverageArgs | Out-Null

    Write-Host ""
    Write-Host "=" * 50 -ForegroundColor DarkGray
    Write-Host " Report: $OutputDir\CodeCoverage_summary.html" -ForegroundColor Green
    Write-Host "=" * 50 -ForegroundColor DarkGray
    Write-Host ""

    # Open report if requested
    if ($OpenReport) {
        $ReportPath = Join-Path $ScriptDir "$OutputDir\CodeCoverage_summary.html"
        if (Test-Path $ReportPath) {
            Start-Process $ReportPath
        }
    }
}
finally {
    Pop-Location
}
