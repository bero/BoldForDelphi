{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
unit TestInvoicingScenarioFPC;
{ FPCUnit twin of ../Tests/Test.InvoicingScenario.pas (DUnitX). The same three
  tests over the shared scenario in InvoicingScenario.pas. Like the DUnitX
  fixture it collects every failed check and fails the test once with all of
  them listed. Each test gets a fresh XML data file, deleted in TearDown. }

interface

uses
  SysUtils,
  Classes,
  fpcunit,
  testregistry,
  InvoicingSystemFPC;

type
  TTestInvoicingScenario = class(TTestCase)
  private
    FInvoicing: TInvoicingSystem;
    FDataFile: string;
    procedure OpenDatabase;
    procedure AssertAllChecksPassed;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AbstractCustomerIsRefused;
    procedure BusinessMethodsAndDerivations;
    procedure SurvivesSaveCloseAndReopen;
  end;

implementation

uses
  InvoicingScenario;

var
  // The scenario callback is a plain procedure, so the collected results live
  // at unit level. FPCUnit runs the tests one after the other; SetUp resets them.
  ChecksRun: Integer;
  Failures: TStringList;

procedure CollectCheck(const AWhat: string; APassed: Boolean; const AActual: string);
begin
  Inc(ChecksRun);
  if not APassed then
    Failures.Add(AWhat + ' = ' + AActual);
end;

function ModelFileName: string;
begin
  Result := ExpandFileName(ExtractFilePath(ParamStr(0)) +
    '..' + PathDelim + '..' + PathDelim + 'Model' + PathDelim + 'Invoicing.bld');
end;

procedure TTestInvoicingScenario.SetUp;
begin
  ChecksRun := 0;
  Failures.Clear;
  FInvoicing := TInvoicingSystem.Create;
  FInvoicing.LoadModelFromFile(ModelFileName);
end;

procedure TTestInvoicingScenario.TearDown;
begin
  // Destroying the system object closes the system and discards what the test
  // left unsaved.
  FreeAndNil(FInvoicing);
  if (FDataFile <> '') and FileExists(FDataFile) then
    DeleteFile(FDataFile);
  FDataFile := '';
end;

procedure TTestInvoicingScenario.OpenDatabase;
begin
  // A fresh name per test; Bold's file persistence treats a missing file as
  // an empty database.
  FDataFile := GetTempFileName('', 'invoicing') + '.xml';
  FInvoicing.OpenSystem(FDataFile);
end;

procedure TTestInvoicingScenario.AssertAllChecksPassed;
begin
  AssertTrue('The scenario ran no checks', ChecksRun > 0);
  AssertEquals(Format('%d of %d checks failed:' + LineEnding + '%s',
    [Failures.Count, ChecksRun, Failures.Text]), 0, Failures.Count);
end;

procedure TTestInvoicingScenario.AbstractCustomerIsRefused;
begin
  OpenDatabase;
  CheckAbstractCustomerRefused(FInvoicing.BoldSystem, CollectCheck);
  AssertAllChecksPassed;
end;

procedure TTestInvoicingScenario.BusinessMethodsAndDerivations;
begin
  OpenDatabase;
  BuildAndVerifyScenario(FInvoicing.BoldSystem, CollectCheck);
  // Saving proves the store takes the whole object graph, not only that
  // the derivations evaluate.
  FInvoicing.Save;
  AssertAllChecksPassed;
end;

procedure TTestInvoicingScenario.SurvivesSaveCloseAndReopen;
begin
  OpenDatabase;
  BuildAndVerifyScenario(FInvoicing.BoldSystem, CollectCheck);
  FInvoicing.Save;
  FInvoicing.CloseSystem(False);
  FInvoicing.OpenSystem(FDataFile);
  VerifyReloadedScenario(FInvoicing.BoldSystem, CollectCheck);
  AssertAllChecksPassed;
end;

initialization
  Failures := TStringList.Create;
  RegisterTest(TTestInvoicingScenario);

finalization
  Failures.Free;

end.
