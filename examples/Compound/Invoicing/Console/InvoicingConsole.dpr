program InvoicingConsole;

{ Headless runner for the Invoicing example.

  It proves without any GUI that the data module opens a Bold system on SQLite
  with nothing but the model file, and that the business methods, derived
  attributes and constraints behave. The scenario itself lives in the shared
  unit InvoicingScenario, so the DUnitX tests and a future Lazarus runner
  execute exactly the same checks; this program only opens the system, prints
  one line per check and returns the number of failures as its exit code.

  Usage: InvoicingConsole [path\to\Invoicing.bld]
  The database InvoicingConsole.db is recreated next to the exe on every run. }

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.IOUtils,
  FireDAC.ConsoleUI.Wait,
  dmInvoicing in '..\Data\dmInvoicing.pas' {dmInvoicing: TDataModule},
  InvoicingClasses in '..\Model\InvoicingClasses.pas',
  InvoicingScenario in '..\Tests\InvoicingScenario.pas';

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
    Result := TPath.GetFullPath(TPath.Combine(ExtractFilePath(ParamStr(0)), '..\Model\Invoicing.bld'));
end;

function DatabaseFileName: string;
begin
  Result := ChangeFileExt(ParamStr(0), '.db');
end;

procedure Run;
var
  dm: TdmInvoicing;
begin
  if FileExists(DatabaseFileName) then
    TFile.Delete(DatabaseFileName);

  dm := TdmInvoicing.Create(nil);
  try
    Writeln('Model:    ', ModelFileName);
    Writeln('Database: ', DatabaseFileName);
    dm.LoadModelFromFile(ModelFileName);
    dm.OpenSystem(DatabaseFileName);
    Writeln('Scenario');
    CheckAbstractCustomerRefused(dm.BoldSystem, Check);
    BuildAndVerifyScenario(dm.BoldSystem, Check);
    dm.Save;
    dm.CloseSystem(False);

    dm.OpenSystem(DatabaseFileName);
    Writeln('After save, close and reopen');
    VerifyReloadedScenario(dm.BoldSystem, Check);
    dm.CloseSystem(False);
  finally
    dm.Free;
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
      ExitCode := 1;
    end;
  end;
end.
