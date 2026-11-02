unit BoldTestCaseDB;

{******************************************************************************}
{                                                                              }
{  BoldTestCaseDB - persistence test base on the runner's database             }
{                                                                              }
{  A TBoldTestCasePersistence whose connection and database adapter come from  }
{  the connector that the test runner links in, so one test unit runs on       }
{  FireDAC under Delphi and on Zeos under Free Pascal:                         }
{    Delphi:      BoldTestCaseDBFireDAC (UnitTest.dpr): FireDAC, configured    }
{                 from UnitTest.ini like TBoldTestCaseFireDAC                  }
{    Free Pascal: BoldTestCaseDBZeos (FPC/UnitTestFPC.lpr): Zeos, shared-cache }
{                 in-memory SQLite like TBoldTestCaseZeos                      }
{  Each connector registers itself in its initialization, as the connectors    }
{  of dmBoldTest and dmUndoRedo do. A test that needs one specific adapter     }
{  inherits TBoldTestCaseFireDAC, TBoldTestCaseUniDAC or TBoldTestCaseZeos     }
{  instead.                                                                    }
{                                                                              }
{******************************************************************************}

interface

uses
  Classes,
  BoldAbstractDatabaseAdapter,
  BoldTestCasePersistence;

type
  { One instance per test, created before the connection and freed after the
    base class has freed connection and adapter. The constructor prepares the
    database (it may create it, or keep an in-memory one alive). }
  TBoldTestCaseDBConnector = class
  public
    constructor Create; virtual;
    function CreateConnection: TComponent; virtual; abstract;
    function CreateDatabaseAdapter(AConnection: TComponent): TBoldAbstractDatabaseAdapter; virtual; abstract;
    procedure OpenConnection(AConnection: TComponent); virtual; abstract;
    procedure CloseConnection(AConnection: TComponent); virtual; abstract;
  end;

  TBoldTestCaseDBConnectorClass = class of TBoldTestCaseDBConnector;

  TBoldTestCaseDB = class(TBoldTestCasePersistence)
  private
    FConnector: TBoldTestCaseDBConnector;
  protected
    function CreateConnection: TComponent; override;
    function CreateDatabaseAdapter(AConnection: TComponent): TBoldAbstractDatabaseAdapter; override;
    procedure OpenConnection; override;
    procedure CloseConnection; override;

    procedure SetUp; override;
    procedure TearDown; override;
  end;

procedure RegisterBoldTestCaseDBConnector(AConnectorClass: TBoldTestCaseDBConnectorClass);

implementation

uses
  SysUtils;

var
  RegisteredConnectorClass: TBoldTestCaseDBConnectorClass = nil;

procedure RegisterBoldTestCaseDBConnector(AConnectorClass: TBoldTestCaseDBConnectorClass);
begin
  RegisteredConnectorClass := AConnectorClass;
end;

{ TBoldTestCaseDBConnector }

constructor TBoldTestCaseDBConnector.Create;
begin
  inherited Create;
end;

{ TBoldTestCaseDB }

procedure TBoldTestCaseDB.SetUp;
begin
  if not Assigned(RegisteredConnectorClass) then
    raise Exception.Create('BoldTestCaseDB: no database connector is registered. The test runner must ' +
      'use BoldTestCaseDBFireDAC (Delphi) or BoldTestCaseDBZeos (Free Pascal).');
  FConnector := RegisteredConnectorClass.Create;
  inherited;
end;

procedure TBoldTestCaseDB.TearDown;
begin
  try
    inherited; // closes and frees the connection and the adapter
  finally
    FreeAndNil(FConnector);
  end;
end;

function TBoldTestCaseDB.CreateConnection: TComponent;
begin
  Result := FConnector.CreateConnection;
end;

function TBoldTestCaseDB.CreateDatabaseAdapter(AConnection: TComponent): TBoldAbstractDatabaseAdapter;
begin
  Result := FConnector.CreateDatabaseAdapter(AConnection);
end;

procedure TBoldTestCaseDB.OpenConnection;
begin
  FConnector.OpenConnection(Connection);
end;

procedure TBoldTestCaseDB.CloseConnection;
begin
  FConnector.CloseConnection(Connection);
end;

end.
