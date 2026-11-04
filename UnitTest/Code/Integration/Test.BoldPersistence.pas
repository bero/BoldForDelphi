unit Test.BoldPersistence;

{ The database operation tests (BoldDatabaseOperationsTestCase) on FireDAC: a
  private in-memory SQLite database per test, with no Bold system. Delphi only;
  the Free Pascal counterpart on Zeos is Zeos\Test.BoldPersistenceZeos. }

interface

uses
  DUnitX.TestFramework,
  BoldDBInterfaces,
  BoldDatabaseAdapterFireDAC,
  FireDAC.Comp.Client,
  BoldDatabaseOperationsTestCase;

type
  [Category('Integration')]
  [Category('Database')]
  TTestBoldDatabaseOperations = class(TBoldDatabaseOperationsTestCase)
  private
    FConnection: TFDConnection;
    FAdapter: TBoldDatabaseAdapterFireDAC;
  protected
    function OpenDatabase: IBoldDatabase; override;
    procedure CloseDatabase; override;
  end;

implementation

uses
  System.SysUtils,
  BoldTestFramework,
  FireDAC.Stan.Def,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef;

{ TTestBoldDatabaseOperations }

function TTestBoldDatabaseOperations.OpenDatabase: IBoldDatabase;
begin
  // Create connection directly (bypassing Bold system)
  FConnection := TFDConnection.Create(nil);
  FConnection.DriverName := 'SQLite';
  FConnection.Params.Values['Database'] := ':memory:';
  FConnection.LoginPrompt := False;

  FAdapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  FAdapter.Connection := FConnection;

  FConnection.Open;
  Result := FAdapter.DatabaseInterface;
end;

procedure TTestBoldDatabaseOperations.CloseDatabase;
begin
  FreeAndNil(FAdapter);
  FreeAndNil(FConnection);
end;

initialization
  BoldRegisterTest(TTestBoldDatabaseOperations);

end.
