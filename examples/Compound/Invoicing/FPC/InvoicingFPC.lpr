{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
program InvoicingFPC;
{ Free Pascal / Lazarus runner for the shared Invoicing acceptance scenario
  (../Tests/InvoicingScenario.pas), the FPC counterpart of
  ../Console/InvoicingConsole.dpr. It loads ../Model/Invoicing.bld, runs the
  scenario, saves, closes, reopens and verifies the reloaded objects. The data
  is stored in an XML file (InvoicingSystemFPC.pas); the Delphi console uses
  SQLite through FireDAC. One line per check; exit code = number of failed
  checks.

  Build and run on Linux with build.sh next to this file (FPC 3.2.2 or later). }

{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  SysUtils,
  InvoicingClasses,
  InvoicingScenario,
  InvoicingSystemFPC;

var
  Failures: Integer = 0;

procedure Check(const AWhat: string; APassed: Boolean; const AActual: string);
begin
  if APassed then
    Writeln('  OK    ', AWhat, ' = ', AActual)
  else
  begin
    Writeln('  FAIL  ', AWhat, ' = ', AActual);
    Inc(Failures);
  end;
end;

function ModelFileName: string;
begin
  if ParamCount >= 1 then
    Result := ParamStr(1)
  else
    Result := ExpandFileName(ExtractFilePath(ParamStr(0)) +
      '..' + PathDelim + '..' + PathDelim + 'Model' + PathDelim + 'Invoicing.bld');
end;

function DataFileName: string;
begin
  Result := ChangeFileExt(ParamStr(0), '.xml');
end;

procedure Run;
var
  Invoicing: TInvoicingSystem;
begin
  if FileExists(DataFileName) then
    DeleteFile(DataFileName);

  Invoicing := TInvoicingSystem.Create;
  try
    Writeln('Model:    ', ModelFileName);
    Writeln('Database: ', DataFileName);
    Invoicing.LoadModelFromFile(ModelFileName);
    Invoicing.OpenSystem(DataFileName);
    Writeln('Scenario');
    CheckAbstractCustomerRefused(Invoicing.BoldSystem, Check);
    BuildAndVerifyScenario(Invoicing.BoldSystem, Check);
    Invoicing.Save;
    Invoicing.CloseSystem(False);

    Invoicing.OpenSystem(DataFileName);
    Writeln('After save, close and reopen');
    VerifyReloadedScenario(Invoicing.BoldSystem, Check);
    Invoicing.CloseSystem(False);
  finally
    Invoicing.Free;
  end;
end;

begin
  try
    Run;
    if Failures = 0 then
      Writeln('All checks passed')
    else
      Writeln(Failures, ' check(s) FAILED');
    ExitCode := Failures;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 255;
    end;
  end;
end.
