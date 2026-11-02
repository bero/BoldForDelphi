unit BoldTestCaseDBFireDAC;

{ The database for TBoldTestCaseDB under Delphi: FireDAC, configured from
  UnitTest.ini (or BOLD_TEST_ENGINE) exactly as TBoldTestCaseFireDAC does.
  Linking this unit (UnitTest.dpr) registers it as TBoldTestCaseDB's
  connector; the Free Pascal counterpart is Zeos\BoldTestCaseDBZeos. }

interface

implementation

uses
  System.Classes,
  BoldAbstractDatabaseAdapter,
  BoldDatabaseAdapterFireDAC,
  BoldTestDatabaseConfig,
  BoldTestCaseDB,
  FireDAC.Comp.Client;

type
  TBoldTestCaseDBConnectorFireDAC = class(TBoldTestCaseDBConnector)
  public
    constructor Create; override;
    function CreateConnection: TComponent; override;
    function CreateDatabaseAdapter(AConnection: TComponent): TBoldAbstractDatabaseAdapter; override;
    procedure OpenConnection(AConnection: TComponent); override;
    procedure CloseConnection(AConnection: TComponent); override;
  end;

constructor TBoldTestCaseDBConnectorFireDAC.Create;
begin
  inherited;
  // Ensure the test database exists before the connection opens
  CreateTestDatabase;
end;

function TBoldTestCaseDBConnectorFireDAC.CreateConnection: TComponent;
begin
  // Configured in CreateDatabaseAdapter via ConfigureConnection
  Result := TFDConnection.Create(nil);
end;

function TBoldTestCaseDBConnectorFireDAC.CreateDatabaseAdapter(AConnection: TComponent): TBoldAbstractDatabaseAdapter;
var
  LAdapter: TBoldDatabaseAdapterFireDAC;
begin
  LAdapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  LAdapter.Connection := AConnection as TFDConnection;
  ConfigureConnection(AConnection as TFDConnection, LAdapter);
  Result := LAdapter;
end;

procedure TBoldTestCaseDBConnectorFireDAC.OpenConnection(AConnection: TComponent);
begin
  (AConnection as TFDConnection).Open;
end;

procedure TBoldTestCaseDBConnectorFireDAC.CloseConnection(AConnection: TComponent);
begin
  if (AConnection as TFDConnection).Connected then
    (AConnection as TFDConnection).Close;
end;

initialization
  RegisterBoldTestCaseDBConnector(TBoldTestCaseDBConnectorFireDAC);

end.
