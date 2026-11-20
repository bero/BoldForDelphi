# Bold for Delphi - VCL-free runtime check (#117, plan step 6)
#
# Builds three FMX-free or FMX programs with a detailed map file and fails when
# the map of any names a Vcl.* unit:
#   - InvoicingConsole (examples\Compound\Invoicing\Console), FrameworkType=None:
#     the Bold runtime closure (object space, OCL, handles, FireDAC persistence,
#     XML streaming).
#   - InvoicingFMX (examples\Compound\Invoicing\FMX), FrameworkType=FMX with
#     BOLD_NO_VCL: on top of that TBoldDataSet, the control packs it builds on
#     and LiveBindings (#117 step 9).
#   - ModelEditorFMX (Tools\ModelEditor\FMX), FrameworkType=FMX with BOLD_NO_VCL:
#     the standalone model editor's FMX client (#128), with the UML model, the
#     code generator and the data module it shares with the LCL client.
# The Bold core links no GUI framework; a VCL application links
# BoldEnvironmentVCL itself. Run it after a change to Source: the next unit that
# imports a VCL unit then stops here instead of going unnoticed until an FMX,
# service or Free Pascal build breaks. An FMX project knows no Vcl unit scope,
# so an unscoped VCL import (Controls, Forms, Graphics) outside BOLD_VCL already
# fails its build with F2613; a scoped one (Vcl.Controls) builds and is caught
# in the map file.
#
# USAGE:
#   .\check_no_vcl.ps1 [-Project All|Console|FMX|ModelEditor] [-Platform Win64] [-Config Debug] [-DelphiVersion 37.0] [-SkipBuild] [-Run]
#
# PARAMETERS:
#   -Project       : All (default), Console, FMX or ModelEditor.
#   -Platform      : Win32 or Win64 (default: Win64).
#   -Config        : Debug or Release (default: Debug).
#   -DelphiVersion : Registry version of the Delphi to use (e.g. 37.0 = 13). Default: newest.
#   -SkipBuild     : Only read the map files of the previous builds.
#   -Run           : Also run the built InvoicingConsole.exe against ..\Model\Invoicing.bld;
#                    its exit code is the number of failed scenario checks. The FMX clients
#                    are GUI programs and are not run.
#
# EXIT CODE: 0 when every build succeeds and no Vcl.* unit is linked (and, with -Run,
# every scenario check passes); 1 otherwise. The offending units are listed.

param(
    [ValidateSet("All", "Console", "FMX", "ModelEditor")]
    [string]$Project = "All",
    [string]$Platform = "Win64",
    [string]$Config = "Debug",
    [string]$DelphiVersion = "",
    [switch]$SkipBuild,
    [switch]$Run
)

$BuildScript = "C:\Attracs\DelphiStandards\DelphiBuildDPROJ.ps1"
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RepoRoot = Split-Path -Parent $ScriptDir
$InvoicingDir = Join-Path $RepoRoot "examples\Compound\Invoicing"

$Projects = @(@(
    @{ Key = "Console"; Name = "InvoicingConsole"; Dir = Join-Path $InvoicingDir "Console";
       What = "FrameworkType=None" },
    @{ Key = "FMX"; Name = "InvoicingFMX"; Dir = Join-Path $InvoicingDir "FMX";
       What = "FrameworkType=FMX, BOLD_NO_VCL" },
    @{ Key = "ModelEditor"; Name = "ModelEditorFMX"; Dir = Join-Path $RepoRoot "Tools\ModelEditor\FMX";
       What = "FrameworkType=FMX, BOLD_NO_VCL" }
) | Where-Object { $Project -eq "All" -or $_.Key -eq $Project })

if (-not $SkipBuild -and -not (Test-Path $BuildScript)) {
    Write-Host "ERROR: Build script not found: $BuildScript" -ForegroundColor Red
    exit 1
}

# Builds one project (unless -SkipBuild) and checks its map file; $true when no VCL unit is linked.
# The build output goes to Out-Host, so the function returns only that one value.
function Test-NoVcl($P) {
    $ProjectPath = Join-Path $P.Dir ($P.Name + ".dproj")
    $MapFile = Join-Path $P.Dir ($P.Name + ".map")
    if (-not (Test-Path $ProjectPath)) {
        Write-Host "ERROR: Project file not found: $ProjectPath" -ForegroundColor Red
        return $false
    }
    if (-not $SkipBuild) {
        Remove-Item $MapFile -ErrorAction SilentlyContinue
        Write-Host ("Building {0} ({1}, {2}) with {3}" -f $P.Name, $Config, $Platform, $P.What) -ForegroundColor Cyan
        if ($DelphiVersion) {
            & $BuildScript -ProjectFile $ProjectPath -Config $Config -Platform $Platform -DelphiVersion $DelphiVersion | Out-Host
        } else {
            & $BuildScript -ProjectFile $ProjectPath -Config $Config -Platform $Platform | Out-Host
        }
        if ($LASTEXITCODE -ne 0) {
            Write-Host ("ERROR: {0} build failed (exit {1})" -f $P.Name, $LASTEXITCODE) -ForegroundColor Red
            if ($P.Key -ne "Console") {
                Write-Host "F2613 for Controls, Forms, Graphics or another VCL unit means a Bold unit imports it outside BOLD_VCL." -ForegroundColor Yellow
            }
            return $false
        }
    }
    if (-not (Test-Path $MapFile)) {
        Write-Host "ERROR: Map file not found: $MapFile (the dproj sets DCC_MapFile=3; was the build skipped?)" -ForegroundColor Red
        return $false
    }

    # The detailed map lists every linked unit as M=<unit> in its segment table.
    $Modules = Select-String -Path $MapFile -Pattern '\bM=(\S+)' -AllMatches |
        ForEach-Object { $_.Matches } | ForEach-Object { $_.Groups[1].Value } | Sort-Object -Unique
    $VclUnits = @($Modules | Where-Object { $_ -match '^Vcl\.' })

    Write-Host ("{0} links {1} units; {2} from the VCL" -f $P.Name, $Modules.Count, $VclUnits.Count)
    if ($VclUnits.Count -gt 0) {
        Write-Host ("ERROR: {0} links VCL units again:" -f $P.Name) -ForegroundColor Red
        $VclUnits | ForEach-Object { Write-Host "  $_" -ForegroundColor Red }
        Write-Host "Find the importer with the unit's uses clauses, or compare the map file with the previous build." -ForegroundColor Yellow
        return $false
    }
    Write-Host ("OK: {0} links no VCL unit" -f $P.Name) -ForegroundColor Green
    return $true
}

$Failed = 0
$ConsoleOk = $false
foreach ($P in $Projects) {
    $Ok = Test-NoVcl $P
    if (-not $Ok) { $Failed++ }
    if ($P.Key -eq "Console") { $ConsoleOk = $Ok }
}

if ($Run) {
    if ($ConsoleOk) {
        $ConsoleDir = Join-Path $InvoicingDir "Console"
        $ExeFile = Join-Path $ConsoleDir "InvoicingConsole.exe"
        $ModelFile = Join-Path $ConsoleDir "..\Model\Invoicing.bld"
        Write-Host "Running the scenario: $ExeFile $ModelFile" -ForegroundColor Cyan
        & $ExeFile $ModelFile
        if ($LASTEXITCODE -ne 0) {
            Write-Host "ERROR: $LASTEXITCODE scenario check(s) failed" -ForegroundColor Red
            $Failed++
        } else {
            Write-Host "OK: all scenario checks passed" -ForegroundColor Green
        }
    } elseif ($Project -ne "All") {
        Write-Host "-Run applies to InvoicingConsole only; nothing run" -ForegroundColor Yellow
    } else {
        Write-Host "The scenario is not run: the InvoicingConsole check failed" -ForegroundColor Yellow
    }
}

if ($Failed -gt 0) { exit 1 }
exit 0
