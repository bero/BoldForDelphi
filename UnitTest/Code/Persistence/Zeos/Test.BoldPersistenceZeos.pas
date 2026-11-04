unit Test.BoldPersistenceZeos;

{ The database operation tests (BoldDatabaseOperationsTestCase) on Zeos under
  Free Pascal: a private in-memory SQLite database per test, with no Bold
  system. The Delphi counterpart on FireDAC is Test.BoldPersistence. }

interface

uses
  ZConnection,
  BoldDBInterfaces,
  BoldDatabaseAdapterZeos,
  BoldDatabaseOperationsTestCase;

type
  TTestBoldDatabaseOperationsZeos = class(TBoldDatabaseOperationsTestCase)
  private
    FConnection: TZConnection;
    FAdapter: TBoldDatabaseAdapterZeos;
  protected
    function OpenDatabase: IBoldDatabase; override;
    procedure CloseDatabase; override;
  end;

implementation

uses
  SysUtils,
  BoldTestFramework,
  BoldTestZeos;

var
  DatabaseCounter: Integer = 0;

{ TTestBoldDatabaseOperationsZeos }

function TTestBoldDatabaseOperationsZeos.OpenDatabase: IBoldDatabase;
begin
  // A new name per test: an in-memory database of its own, gone when its only
  // connection closes, like FireDAC's :memory:.
  inc(DatabaseCounter);
  FConnection := NewTestZeosConnection(nil, 'file:boldpersistencetest' + IntToStr(DatabaseCounter));
  FAdapter := NewTestZeosAdapter(nil, FConnection);
  FConnection.Connect;
  Result := FAdapter.DatabaseInterface;
end;

procedure TTestBoldDatabaseOperationsZeos.CloseDatabase;
begin
  FreeAndNil(FAdapter);
  FreeAndNil(FConnection);
end;

initialization
  BoldRegisterTest(TTestBoldDatabaseOperationsZeos);

end.
