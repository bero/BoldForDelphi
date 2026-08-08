# Bold for Delphi - Unit Test Dependency Setup
#
# Clones the two external frameworks required by UnitTest.dproj / UnitTestGUI.dproj
# and sets the environment variables the projects reference in their search path:
#
#   DUnitX      -> <clone>\DUnitX\Source        (https://github.com/VSoftTechnologies/DUnitX)
#   DelphiMocks -> <clone>\Delphi-Mocks\Source  (https://github.com/VSoftTechnologies/Delphi-Mocks)
#
# USAGE:
#   .\setup-tests.ps1 [-DependencyRoot "C:\SomeFolder"] [-SkipEnvVars]
#
# PARAMETERS:
#   -DependencyRoot : Folder to clone the frameworks into.
#                     Default: the parent folder of this repository, so the
#                     clones end up as siblings of BoldForDelphi.
#   -SkipEnvVars    : Only clone; do not write user-level environment variables.
#
# NOTE: After running this script, restart the Delphi IDE - it reads
# environment variables at startup. Alternatively, set the variables inside
# the IDE under Tools > Options > IDE > Environment Variables.

param(
    [string]$DependencyRoot = "",
    [switch]$SkipEnvVars
)

$ErrorActionPreference = "Stop"

# Repo root = parent of the UnitTest folder this script lives in
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RepoRoot = Split-Path -Parent $ScriptDir

if ($DependencyRoot -eq "") {
    $DependencyRoot = Split-Path -Parent $RepoRoot
}

if (-not (Test-Path $DependencyRoot)) {
    New-Item -ItemType Directory -Force $DependencyRoot | Out-Null
}

$Dependencies = @(
    @{ Name = "DUnitX";       EnvVar = "DUnitX";      Url = "https://github.com/VSoftTechnologies/DUnitX.git" },
    @{ Name = "Delphi-Mocks"; EnvVar = "DelphiMocks"; Url = "https://github.com/VSoftTechnologies/Delphi-Mocks.git" }
)

Write-Host "Bold for Delphi - unit test dependency setup" -ForegroundColor Cyan
Write-Host "  Dependency root: $DependencyRoot" -ForegroundColor Gray
Write-Host ""

foreach ($Dep in $Dependencies) {
    $ExistingValue = [Environment]::GetEnvironmentVariable($Dep.EnvVar, "User")

    if ($ExistingValue -and (Test-Path $ExistingValue)) {
        # Already configured and valid - leave it alone
        Write-Host "$($Dep.EnvVar) already set to $ExistingValue - keeping it." -ForegroundColor Green
        Set-Item -Path "env:$($Dep.EnvVar)" -Value $ExistingValue
        continue
    }

    $ClonePath = Join-Path $DependencyRoot $Dep.Name
    $SourcePath = Join-Path $ClonePath "Source"

    if (Test-Path $SourcePath) {
        Write-Host "$($Dep.Name) already present at $ClonePath - skipping clone." -ForegroundColor Green
    } else {
        Write-Host "Cloning $($Dep.Url) ..." -ForegroundColor Cyan
        git clone --depth 1 $Dep.Url $ClonePath
        if ($LASTEXITCODE -ne 0) {
            Write-Host "ERROR: git clone failed for $($Dep.Name)" -ForegroundColor Red
            exit 1
        }
    }

    # Make the variable available to this PowerShell session immediately
    Set-Item -Path "env:$($Dep.EnvVar)" -Value $SourcePath

    if (-not $SkipEnvVars) {
        [Environment]::SetEnvironmentVariable($Dep.EnvVar, $SourcePath, "User")
        Write-Host "$($Dep.EnvVar) = $SourcePath (user environment variable set)" -ForegroundColor Green
    } else {
        Write-Host "$($Dep.EnvVar) = $SourcePath (session only, -SkipEnvVars given)" -ForegroundColor Yellow
    }
}

Write-Host ""
Write-Host "Done. Restart the Delphi IDE so it picks up the new variables," -ForegroundColor Cyan
Write-Host "then open UnitTest\UnitTest.groupproj and build." -ForegroundColor Cyan
Write-Host "Command-line alternative: .\build.ps1" -ForegroundColor Gray
