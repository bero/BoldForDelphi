{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
program InvoicingZeos;
{ Lazarus runner for the shared Invoicing acceptance scenario
  (../Tests/InvoicingScenario.pas) on a SQL database: SQLite through Zeos and
  Bold's Zeos adapter (InvoicingSystemZeos.pas). It is the Free Pascal twin of
  ../Console/InvoicingConsole.dpr, which uses SQLite through FireDAC, and of
  ../FPC/InvoicingFPC.lpr, which stores in an XML file. It loads
  ../Model/Invoicing.bld, runs the scenario, saves, closes, reopens and
  verifies the reloaded objects. One line per check; exit code = number of
  failed checks.

  InvoicingZeos.lpi requires the Lazarus packages Bold and BoldZeos
  (packages/Lazarus). Build and run on Linux with build.sh next to this file. }

{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  SysUtils,
  InvoicingClasses,
  InvoicingScenario,
  InvoicingSystemZeos;

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
  Result := ChangeFileExt(ParamStr(0), '.db');
end;

procedure Run;
var
  Invoicing: TInvoicingSystemZeos;
begin
  if FileExists(DataFileName) then
    DeleteFile(DataFileName);

  Invoicing := TInvoicingSystemZeos.Create;
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
