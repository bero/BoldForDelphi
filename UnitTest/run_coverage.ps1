# Bold for Delphi - Code Coverage Script
# Uses DelphiBuildDPROJ.ps1 for flexible Delphi version detection
#
# USAGE:
#   .\run_coverage.ps1 [-SkipBuild] [-OpenReport:$false] [-Upload] [-Quick]
#
# PARAMETERS:
#   -SkipBuild        : Skip build, only run coverage analysis on existing executable
#   -OpenReport:$false: Suppress opening the coverage report (opens by default)
#   -Upload           : Upload coverage to Codecov.io (requires CODECOV_TOKEN env var)
#   -Quick            : Run only tests in the 'Quick' category
#
# EXAMPLES:
#   .\run_coverage.ps1                    # Build, run coverage, open report
#   .\run_coverage.ps1 -SkipBuild         # Skip build, run coverage, open report
#   .\run_coverage.ps1 -OpenReport:$false # Build, run coverage, don't open report
#   .\run_coverage.ps1 -Upload            # Build, run coverage, open report, upload
#   .\run_coverage.ps1 -Quick             # Build, run only Quick tests, open report

param(
    [switch]$SkipBuild,
    [bool]$OpenReport = $true,
    [switch]$Upload,
    [switch]$Quick
)

$ErrorActionPreference = "Stop"

# Configuration
$CoverageExe = "C:\Attracs\DelphiCodeCoverage\build\Win32\CodeCoverage.exe"
$ProjectName = "UnitTest"
$Executable = "$ProjectName.exe"
$MapFile = "$ProjectName.map"
$SourceDir = "..\Source"
$OutputDir = "coverage_report"
$BuildScript = "C:\Attracs\DelphiStandards\DelphiBuildDPROJ.ps1"

# UnitTest project directory
$ProjectDir = "C:\Attracs\BoldForDelphi\UnitTest"
Push-Location $ProjectDir

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

        $ProjectPath = Join-Path $ProjectDir "$ProjectName.dproj"

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

        # Set DUnitX environment variable (must be after rsvars.bat capture)
        $env:DUnitX = "C:\Attracs\DUnitX\Source"
        Write-Host "  DUnitX: $env:DUnitX" -ForegroundColor Gray

        # Set Delphi-Mocks environment variable
        $env:DelphiMocks = "C:\Attracs\Delphi-Mocks\Source"
        Write-Host "  DelphiMocks: $env:DelphiMocks" -ForegroundColor Gray

        # Build with MAP file generation (DCC_MapFile=3 for detailed MAP)
        $MSBuildArgs = @(
            "$ProjectName.dproj",
            "/t:Build",
            "/p:Config=Debug",
            "/p:Platform=Win32",
            "/p:DCC_MapFile=3",
            "/p:DCC_Define=DEBUG",
            "/v:minimal"
        )

        Write-Host "  Running MSBuild..." -ForegroundColor Gray
        & msbuild @MSBuildArgs
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

    # Run coverage analysis (this also runs the tests)
    Write-Host "[COVERAGE] Running tests with code coverage..." -ForegroundColor Yellow
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
        "-od", $OutputDir,
        "-tec"  # Pass through test executable exit code
    )

    # Add Quick category filter if requested
    if ($Quick) {
        Write-Host "  Running Quick category tests only" -ForegroundColor Gray
        $CoverageArgs += "-a"
        $CoverageArgs += "^-^-include:Quick"  # Escape dashes for CodeCoverage parser
    }

    & $CoverageExe @CoverageArgs
    $TestExitCode = $LASTEXITCODE

    Write-Host ""

    if ($TestExitCode -ne 0) {
        Write-Host "=" * 50 -ForegroundColor DarkGray
        Write-Host " ERROR: Tests failed with exit code $TestExitCode" -ForegroundColor Red
        Write-Host "=" * 50 -ForegroundColor DarkGray
        Write-Host ""
        exit $TestExitCode
    }

    Write-Host "=" * 50 -ForegroundColor DarkGray
    Write-Host " Report: $OutputDir\CodeCoverage_summary.html" -ForegroundColor Green
    Write-Host "=" * 50 -ForegroundColor DarkGray
    Write-Host ""

    # Open report if requested
    if ($OpenReport) {
        $ReportPath = Join-Path $ProjectDir "$OutputDir\CodeCoverage_summary.html"
        if (Test-Path $ReportPath) {
            Start-Process $ReportPath
        }
    }

    # Upload to Codecov if requested
    if ($Upload) {
        Write-Host "[CODECOV] Uploading coverage to Codecov.io..." -ForegroundColor Yellow
        Write-Host ""

        if (-not $env:CODECOV_TOKEN) {
            Write-Host "ERROR: CODECOV_TOKEN environment variable not set" -ForegroundColor Red
            Write-Host "       Set it with: `$env:CODECOV_TOKEN = 'your-token-here'" -ForegroundColor Yellow
            exit 1
        }

        # Convert to Codecov JSON format (parses HTML for line-level data)
        $ConverterScript = Join-Path $ProjectDir "Convert-ToCodecovJson.ps1"
        $CodecovFile = Join-Path $ProjectDir "codecov.json"
        
        if (-not (Test-Path $ConverterScript)) {
            Write-Host "ERROR: Converter script not found: $ConverterScript" -ForegroundColor Red
            exit 1
        }

        Write-Host "  Converting to Codecov JSON format..." -ForegroundColor Gray
        & $ConverterScript -OutputFile $CodecovFile

        # Download Codecov uploader if not present
        $CodecovExe = Join-Path $ProjectDir "codecov.exe"
        if (-not (Test-Path $CodecovExe)) {
            Write-Host "  Downloading Codecov uploader..." -ForegroundColor Gray
            Invoke-WebRequest -Uri "https://uploader.codecov.io/latest/windows/codecov.exe" -OutFile $CodecovExe
        }

        # Upload to Codecov
        Write-Host "  Uploading to Codecov..." -ForegroundColor Gray
        & $CodecovExe -t $env:CODECOV_TOKEN -f $CodecovFile -r "bero/BoldForDelphi" -B "develop"
        $UploadExitCode = $LASTEXITCODE

        Write-Host ""
        if ($UploadExitCode -eq 0) {
            Write-Host "[CODECOV] Upload successful!" -ForegroundColor Green
            Write-Host "          View at: https://app.codecov.io/gh/bero/BoldForDelphi" -ForegroundColor Cyan
        } else {
            Write-Host "[CODECOV] Upload failed with exit code $UploadExitCode" -ForegroundColor Red
        }
        Write-Host ""
    }
}
finally {
    Pop-Location
}
