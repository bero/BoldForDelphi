unit BoldTestCaseFireDAC;

{******************************************************************************}
{                                                                              }
{  BoldTestCaseFireDAC - FireDAC persistence testing base class                }
{                                                                              }
{  Concrete implementation of TBoldTestCasePersistence using FireDAC.          }
{  Database configuration is read from UnitTest.ini via BoldTestDatabaseConfig.}
{                                                                              }
{******************************************************************************}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Data.DB,
  BoldTestCasePersistence,
  BoldAbstractDatabaseAdapter,
  BoldDatabaseAdapterFireDAC,
  FireDAC.Comp.Client;

type
  /// <summary>
  /// Base class for Bold test cases using FireDAC.
  /// Database configuration is read from UnitTest.ini.
  /// Supports SQL Server, SQLite, Interbase, Firebird, PostgreSQL.
  /// </summary>
  TBoldTestCaseFireDAC = class(TBoldTestCasePersistence)
  private
    function GetFDConnection: TFDConnection;
    function GetFireDACAdapter: TBoldDatabaseAdapterFireDAC;
  protected
    function CreateConnection: TCustomConnection; override;
    function CreateDatabaseAdapter(AConnection: TCustomConnection): TBoldAbstractDatabaseAdapter; override;

    property FDConnection: TFDConnection read GetFDConnection;
    property FireDACAdapter: TBoldDatabaseAdapterFireDAC read GetFireDACAdapter;
  public
    [Setup]
    procedure SetUp; override;
  end;

implementation

uses
  BoldTestDatabaseConfig,
  BoldSQLDatabaseConfig,
  FireDAC.Stan.Def,
  FireDAC.Stan.Async,
  FireDAC.Phys.MSSQL,
  FireDAC.Phys.MSSQLDef,
  FireDAC.Phys.IB,
  FireDAC.Phys.IBDef,
  FireDAC.Phys.FB,
  FireDAC.Phys.FBDef,
  FireDAC.Phys.PG,
  FireDAC.Phys.PGDef,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef;

{ TBoldTestCaseFireDAC }

procedure TBoldTestCaseFireDAC.SetUp;
begin
  // Ensure test database exists before opening connection
  CreateTestDatabase;
  inherited;
end;

function TBoldTestCaseFireDAC.CreateConnection: TCustomConnection;
var
  LConnection: TFDConnection;
begin
  LConnection := TFDConnection.Create(nil);
  // Connection will be configured in CreateDatabaseAdapter via ConfigureConnection
  Result := LConnection;
end;

function TBoldTestCaseFireDAC.CreateDatabaseAdapter(AConnection: TCustomConnection): TBoldAbstractDatabaseAdapter;
var
  LAdapter: TBoldDatabaseAdapterFireDAC;
begin
  LAdapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  LAdapter.Connection := AConnection as TFDConnection;
  // Configure connection and adapter from UnitTest.ini
  ConfigureConnection(AConnection as TFDConnection, LAdapter);
  Result := LAdapter;
end;

function TBoldTestCaseFireDAC.GetFDConnection: TFDConnection;
begin
  Result := Connection as TFDConnection;
end;

function TBoldTestCaseFireDAC.GetFireDACAdapter: TBoldDatabaseAdapterFireDAC;
begin
  Result := DatabaseAdapter as TBoldDatabaseAdapterFireDAC;
end;

end.
