# Bold for Delphi - VCL-free runtime check (#117, plan step 6)
#
# Builds the Invoicing console example (examples\Compound\Invoicing\Console),
# which links the Bold runtime closure (object space, OCL, handles, FireDAC
# persistence, XML streaming) with FrameworkType=None, and then fails when the
# detailed map file of that build names any Vcl.* unit. The Bold core links no
# GUI framework; a VCL application links BoldEnvironmentVCL itself. Run it after a change to Source: the next unit that imports a
# VCL unit then stops here instead of going unnoticed until an FMX, service
# or Free Pascal build breaks.
#
# USAGE:
#   .\check_no_vcl.ps1 [-Platform Win64] [-Config Debug] [-DelphiVersion 37.0] [-SkipBuild] [-Run]
#
# PARAMETERS:
#   -Platform      : Win32 or Win64 (default: Win64).
#   -Config        : Debug or Release (default: Debug).
#   -DelphiVersion : Registry version of the Delphi to use (e.g. 37.0 = 13). Default: newest.
#   -SkipBuild     : Only read the map file of the previous build.
#   -Run           : Also run the built exe against ..\Model\Invoicing.bld; its exit code
#                    is the number of failed scenario checks.
#
# EXIT CODE: 0 when the build succeeds and no Vcl.* unit is linked (and, with -Run,
# every scenario check passes); 1 otherwise. The offending units are listed.

param(
    [string]$Platform = "Win64",
    [string]$Config = "Debug",
    [string]$DelphiVersion = "",
    [switch]$SkipBuild,
    [switch]$Run
)

$BuildScript = "C:\Attracs\DelphiStandards\DelphiBuildDPROJ.ps1"
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RepoRoot = Split-Path -Parent $ScriptDir
$ProjectDir = Join-Path $RepoRoot "examples\Compound\Invoicing\Console"
$ProjectPath = Join-Path $ProjectDir "InvoicingConsole.dproj"
$MapFile = Join-Path $ProjectDir "InvoicingConsole.map"
$ExeFile = Join-Path $ProjectDir "InvoicingConsole.exe"
$ModelFile = Join-Path $ProjectDir "..\Model\Invoicing.bld"

if (-not (Test-Path $ProjectPath)) {
    Write-Host "ERROR: Project file not found: $ProjectPath" -ForegroundColor Red
    exit 1
}

if (-not $SkipBuild) {
    if (-not (Test-Path $BuildScript)) {
        Write-Host "ERROR: Build script not found: $BuildScript" -ForegroundColor Red
        exit 1
    }
    Remove-Item $MapFile -ErrorAction SilentlyContinue
    Write-Host "Building InvoicingConsole ($Config, $Platform) with FrameworkType=None" -ForegroundColor Cyan
    if ($DelphiVersion) {
        & $BuildScript -ProjectFile $ProjectPath -Config $Config -Platform $Platform -DelphiVersion $DelphiVersion
    } else {
        & $BuildScript -ProjectFile $ProjectPath -Config $Config -Platform $Platform
    }
    if ($LASTEXITCODE -ne 0) {
        Write-Host "ERROR: InvoicingConsole build failed (exit $LASTEXITCODE)" -ForegroundColor Red
        exit 1
    }
}

if (-not (Test-Path $MapFile)) {
    Write-Host "ERROR: Map file not found: $MapFile (the dproj sets DCC_MapFile=3; was the build skipped?)" -ForegroundColor Red
    exit 1
}

# The detailed map lists every linked unit as M=<unit> in its segment table.
$Modules = Select-String -Path $MapFile -Pattern '\bM=(\S+)' -AllMatches |
    ForEach-Object { $_.Matches } | ForEach-Object { $_.Groups[1].Value } | Sort-Object -Unique
$VclUnits = @($Modules | Where-Object { $_ -match '^Vcl\.' })

Write-Host ("InvoicingConsole links {0} units; {1} from the VCL" -f $Modules.Count, $VclUnits.Count)
if ($VclUnits.Count -gt 0) {
    Write-Host "ERROR: the Bold runtime links VCL units again:" -ForegroundColor Red
    $VclUnits | ForEach-Object { Write-Host "  $_" -ForegroundColor Red }
    Write-Host "Find the importer with the unit's uses clauses, or compare the map file with the previous build." -ForegroundColor Yellow
    exit 1
}
Write-Host "OK: no VCL unit linked" -ForegroundColor Green

if ($Run) {
    Write-Host "Running the scenario: $ExeFile $ModelFile" -ForegroundColor Cyan
    & $ExeFile $ModelFile
    if ($LASTEXITCODE -ne 0) {
        Write-Host "ERROR: $LASTEXITCODE scenario check(s) failed" -ForegroundColor Red
        exit 1
    }
    Write-Host "OK: all scenario checks passed" -ForegroundColor Green
}
exit 0
