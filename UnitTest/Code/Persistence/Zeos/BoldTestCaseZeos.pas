unit BoldTestCaseZeos;

{******************************************************************************}
{                                                                              }
{  BoldTestCaseZeos - Zeos persistence testing base class (Free Pascal)        }
{                                                                              }
{  Concrete implementation of TBoldTestCasePersistence using Zeos, the         }
{  counterpart of BoldTestCaseFireDAC and BoldTestCaseUniDAC on Delphi.        }
{  The database is the shared-cache in-memory SQLite database of BoldTestZeos, }
{  configured like the SQLite path of the Delphi bases. UnitTest.ini is not    }
{  read: SQLite is the only engine under Free Pascal so far.                   }
{                                                                              }
{******************************************************************************}

interface

uses
  Classes,
  ZConnection,
  BoldTestCasePersistence,
  BoldAbstractDatabaseAdapter,
  BoldDatabaseAdapterZeos;

type
  TBoldTestCaseZeos = class(TBoldTestCasePersistence)
  private
    { A shared-cache in-memory database lives only while a connection to it is
      open, and Bold closes the fixture's connection after creating the schema
      (TBoldSystemSQLMapper.CloseDataBase). This second connection stays open
      for the whole test, as FMemoryDbAnchor does in BoldTestCaseUniDAC. }
    FKeepAlive: TZConnection;
    function GetZConnection: TZConnection;
    function GetZeosAdapter: TBoldDatabaseAdapterZeos;
  protected
    function CreateConnection: TComponent; override;
    function CreateDatabaseAdapter(AConnection: TComponent): TBoldAbstractDatabaseAdapter; override;
    procedure OpenConnection; override;
    procedure CloseConnection; override;

    property ZConnection: TZConnection read GetZConnection;
    property ZeosAdapter: TBoldDatabaseAdapterZeos read GetZeosAdapter;

    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

uses
  SysUtils,
  BoldTestZeos;

{ TBoldTestCaseZeos }

procedure TBoldTestCaseZeos.SetUp;
begin
  FKeepAlive := NewTestZeosKeepAlive(nil);
  inherited;
end;

procedure TBoldTestCaseZeos.TearDown;
begin
  inherited; // frees the fixture connection first
  FreeAndNil(FKeepAlive); // last connection: the in-memory database goes with it
end;

function TBoldTestCaseZeos.CreateConnection: TComponent;
begin
  // The base class owns and frees it
  Result := NewTestZeosConnection(nil);
end;

function TBoldTestCaseZeos.CreateDatabaseAdapter(AConnection: TComponent): TBoldAbstractDatabaseAdapter;
begin
  // The base class owns and frees it
  Result := NewTestZeosAdapter(nil, AConnection as TZConnection);
end;

procedure TBoldTestCaseZeos.OpenConnection;
begin
  ZConnection.Connect;
end;

procedure TBoldTestCaseZeos.CloseConnection;
begin
  if ZConnection.Connected then
    ZConnection.Disconnect;
end;

function TBoldTestCaseZeos.GetZConnection: TZConnection;
begin
  Result := Connection as TZConnection;
end;

function TBoldTestCaseZeos.GetZeosAdapter: TBoldDatabaseAdapterZeos;
begin
  Result := DatabaseAdapter as TBoldDatabaseAdapterZeos;
end;

end.
