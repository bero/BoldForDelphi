unit BoldTestCaseDBZeos;

{ The database for TBoldTestCaseDB under Free Pascal: Zeos on the shared-cache
  in-memory SQLite database of BoldTestZeos, as TBoldTestCaseZeos uses.
  Linking this unit (FPC/UnitTestFPC.lpr) registers it as TBoldTestCaseDB's
  connector; the Delphi counterpart is BoldTestCaseDBFireDAC. }

interface

implementation

uses
  Classes,
  SysUtils,
  ZConnection,
  BoldAbstractDatabaseAdapter,
  BoldTestCaseDB,
  BoldTestZeos;

type
  TBoldTestCaseDBConnectorZeos = class(TBoldTestCaseDBConnector)
  private
    { Bold closes the test's connection after creating the schema, and an
      in-memory database lives only while a connection to it is open. }
    FKeepAlive: TZConnection;
  public
    constructor Create; override;
    destructor Destroy; override;
    function CreateConnection: TComponent; override;
    function CreateDatabaseAdapter(AConnection: TComponent): TBoldAbstractDatabaseAdapter; override;
    procedure OpenConnection(AConnection: TComponent); override;
    procedure CloseConnection(AConnection: TComponent); override;
  end;

constructor TBoldTestCaseDBConnectorZeos.Create;
begin
  inherited;
  FKeepAlive := NewTestZeosKeepAlive(nil);
end;

destructor TBoldTestCaseDBConnectorZeos.Destroy;
begin
  // Freed after the test's connection: the in-memory database goes with it
  FreeAndNil(FKeepAlive);
  inherited;
end;

function TBoldTestCaseDBConnectorZeos.CreateConnection: TComponent;
begin
  Result := NewTestZeosConnection(nil);
end;

function TBoldTestCaseDBConnectorZeos.CreateDatabaseAdapter(AConnection: TComponent): TBoldAbstractDatabaseAdapter;
begin
  Result := NewTestZeosAdapter(nil, AConnection as TZConnection);
end;

procedure TBoldTestCaseDBConnectorZeos.OpenConnection(AConnection: TComponent);
begin
  (AConnection as TZConnection).Connect;
end;

procedure TBoldTestCaseDBConnectorZeos.CloseConnection(AConnection: TComponent);
begin
  if (AConnection as TZConnection).Connected then
    (AConnection as TZConnection).Disconnect;
end;

initialization
  RegisterBoldTestCaseDBConnector(TBoldTestCaseDBConnectorZeos);

end.
