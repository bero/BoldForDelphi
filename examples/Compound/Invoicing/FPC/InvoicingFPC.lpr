{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
program InvoicingFPC;
{ Free Pascal / Lazarus runner for the shared Invoicing acceptance scenario
  (../Tests/InvoicingScenario.pas), the FPC counterpart of
  ../Console/InvoicingConsole.dpr. It loads ../Model/Invoicing.bld, opens an
  in-memory Bold system (no persistence handle) and prints one line per check.
  Exit code = number of failed checks.
  Not run here: VerifyReloadedScenario, which needs a database.

  Build and run on Linux with build.sh next to this file (FPC 3.2.2 or later). }

{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  SysUtils,
  Classes,
  BoldDefs,
  BoldHandles,
  BoldModel,
  BoldSystem,
  BoldSystemHandle,
  InvoicingClasses,
  InvoicingScenario;

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
      '..' + PathDelim + 'Model' + PathDelim + 'Invoicing.bld');
end;

procedure Run;
var
  Model: TBoldModel;
  TypeInfoHandle: TBoldSystemTypeInfoHandle;
  SystemHandle: TBoldSystemHandle;
  Strings: TStringList;
begin
  Model := TBoldModel.Create(nil);
  TypeInfoHandle := TBoldSystemTypeInfoHandle.Create(nil);
  SystemHandle := TBoldSystemHandle.Create(nil);
  Strings := TStringList.Create;
  try
    Writeln('Model:    ', ModelFileName);
    Writeln('Database: none (in-memory system)');
    Strings.LoadFromFile(ModelFileName);
    Model.SetFromModelAsString(Strings);
    TypeInfoHandle.BoldModel := Model;
    SystemHandle.SystemTypeInfoHandle := TypeInfoHandle;
    SystemHandle.Active := True;
    Writeln('Scenario');
    CheckAbstractCustomerRefused(SystemHandle.System, Check);
    BuildAndVerifyScenario(SystemHandle.System, Check);
    SystemHandle.System.Discard;
    SystemHandle.Active := False;
  finally
    Strings.Free;
    SystemHandle.Free;
    TypeInfoHandle.Free;
    Model.Free;
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
