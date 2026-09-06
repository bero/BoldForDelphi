# Bold for Delphi - keep UnitTestGUI.dpr's unit list identical to UnitTest.dpr's
#
# The console runner (UnitTest.dpr) is the source of truth for which test units
# exist. The GUI runner (UnitTestGUI.dpr, TestInsight / DUnitX VCL logger) must
# compile the same units or it silently runs a subset. This script rewrites the
# uses clause of UnitTestGUI.dpr from UnitTest.dpr, swapping the console-only
# TestRunner unit for the GUI logger units, and leaves the program body alone.
#
# USAGE:
#   .\sync_gui_dpr.ps1          # rewrite UnitTestGUI.dpr
#   .\sync_gui_dpr.ps1 -Check   # exit 1 if UnitTestGUI.dpr is out of sync, change nothing

param(
    [switch]$Check
)

$ErrorActionPreference = "Stop"
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ConsoleDpr = Join-Path $ScriptDir "UnitTest.dpr"
$GuiDpr = Join-Path $ScriptDir "UnitTestGUI.dpr"

$Utf8Bom = New-Object System.Text.UTF8Encoding($true)
$Console = [System.IO.File]::ReadAllText($ConsoleDpr)

# The uses clause: from the line "uses" up to and including the line that ends with ";"
$m = [regex]::Match($Console, "(?ms)^uses\r?\n(.*?;)\s*\r?\n")
if (-not $m.Success) { throw "uses clause not found in $ConsoleDpr" }
$Units = $m.Groups[1].Value

# Console-only entry point -> GUI runner units
$RunnerLine = "  TestRunner in 'Code\Main\TestRunner.pas',"
if ($Units -notmatch [regex]::Escape($RunnerLine)) { throw "expected '$RunnerLine' as the first unit of $ConsoleDpr" }
$GuiUnits = $Units.Replace($RunnerLine, "  Forms,`r`n  DUnitX.Loggers.GUI.VCL,`r`n  DUnitX.TestFramework,")

$Gui = @"
program UnitTestGUI;

{ The uses clause below is generated from UnitTest.dpr by sync_gui_dpr.ps1 -
  edit UnitTest.dpr and rerun the script instead of editing this list. }

{`$STRONGLINKTYPES ON}

uses
$GuiUnits

{`$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.Title := 'Bold for Delphi Unit Tests';
  Application.CreateForm(TGUIVCLTestRunner, GUIVCLTestRunner);
  Application.Run;
end.
"@
$Gui = $Gui -replace "(?<!\r)\n", "`r`n"

$Current = if (Test-Path $GuiDpr) { [System.IO.File]::ReadAllText($GuiDpr) } else { "" }
if ($Current -eq $Gui) {
    Write-Host "UnitTestGUI.dpr is in sync with UnitTest.dpr" -ForegroundColor Green
    exit 0
}
if ($Check) {
    Write-Host "UnitTestGUI.dpr is OUT OF SYNC with UnitTest.dpr - run sync_gui_dpr.ps1" -ForegroundColor Red
    exit 1
}
[System.IO.File]::WriteAllText($GuiDpr, $Gui, $Utf8Bom)
$Count = ([regex]::Matches($GuiUnits, "(?m)^\s+[A-Za-z_][\w.]*")).Count
Write-Host "UnitTestGUI.dpr rewritten: $Count units (from UnitTest.dpr)" -ForegroundColor Green
