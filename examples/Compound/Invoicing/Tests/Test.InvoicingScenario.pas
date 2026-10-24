unit Test.InvoicingScenario;

{ DUnitX fixture for the shared acceptance scenario in InvoicingScenario.pas.

  The scenario reports through a plain callback. This fixture collects every
  failed check and fails the test once with all of them listed, so a single run
  shows the whole picture instead of stopping at the first mismatch. The
  console runner prints the same checks line by line; a Lazarus runner would
  wrap the same three procedures in FPCUnit. }

interface

uses
  SysUtils,
  Classes,
  DUnitX.TestFramework,
  dmInvoicing;

type
  [TestFixture]
  [Category('Invoicing')]
  TTestInvoicingScenario = class
  private
    FDataModule: TdmInvoicing;
    FDatabaseFile: string;
    procedure OpenDatabase;
    procedure AssertAllChecksPassed;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure AbstractCustomerIsRefused;
    [Test]
    procedure BusinessMethodsAndDerivations;
    [Test]
    procedure SurvivesSaveCloseAndReopen;
  end;

implementation

uses
  IOUtils,
  InvoicingScenario;

var
  // The scenario callback is a plain procedure, so the collected results live
  // at unit level. DUnitX runs the tests one after the other; SetUp resets them.
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
  Result := TPath.GetFullPath(TPath.Combine(ExtractFilePath(ParamStr(0)), '..\Model\Invoicing.bld'));
end;

{ TTestInvoicingScenario }

procedure TTestInvoicingScenario.SetUp;
begin
  ChecksRun := 0;
  Failures.Clear;
  FDataModule := TdmInvoicing.Create(nil);
  FDataModule.LoadModelFromFile(ModelFileName);
end;

procedure TTestInvoicingScenario.TearDown;
begin
  // Destroying the data module closes the system and discards what the test
  // left unsaved.
  FreeAndNil(FDataModule);

  if (FDatabaseFile <> '') and FileExists(FDatabaseFile) then
    TFile.Delete(FDatabaseFile);

  FDatabaseFile := '';
end;

procedure TTestInvoicingScenario.OpenDatabase;
begin
  // A fresh SQLite file per test, deleted in TearDown. There is no in-memory
  // alternative: FireDAC's SQLite does not interpret URI filenames, so
  // file:x?mode=memory&cache=shared becomes an ordinary file, and a plain
  // :memory: database is private to one connection and dies when it closes,
  // which the reopen test does on purpose. SQLite accepts the empty file
  // GetTempFileName creates as an empty database.
  FDatabaseFile := TPath.GetTempFileName;
  FDataModule.OpenSystem(FDatabaseFile);
end;

procedure TTestInvoicingScenario.AssertAllChecksPassed;
begin
  Assert.IsTrue(ChecksRun > 0, 'The scenario ran no checks');
  Assert.AreEqual(0, Failures.Count,
    Format('%d of %d checks failed:' + sLineBreak + '%s', [Failures.Count, ChecksRun, Failures.Text]));
end;

procedure TTestInvoicingScenario.AbstractCustomerIsRefused;
begin
  OpenDatabase;
  CheckAbstractCustomerRefused(FDataModule.BoldSystem, CollectCheck);
  AssertAllChecksPassed;
end;

procedure TTestInvoicingScenario.BusinessMethodsAndDerivations;
begin
  OpenDatabase;
  BuildAndVerifyScenario(FDataModule.BoldSystem, CollectCheck);
  // Saving proves the database takes the whole object graph, not only that
  // the derivations evaluate.
  FDataModule.Save;
  AssertAllChecksPassed;
end;

procedure TTestInvoicingScenario.SurvivesSaveCloseAndReopen;
begin
  OpenDatabase;
  BuildAndVerifyScenario(FDataModule.BoldSystem, CollectCheck);
  FDataModule.Save;
  FDataModule.CloseSystem(False);
  FDataModule.OpenSystem(FDatabaseFile);
  VerifyReloadedScenario(FDataModule.BoldSystem, CollectCheck);
  AssertAllChecksPassed;
end;

initialization
  Failures := TStringList.Create;
  TDUnitX.RegisterTestFixture(TTestInvoicingScenario);

finalization
  Failures.Free;

end.
