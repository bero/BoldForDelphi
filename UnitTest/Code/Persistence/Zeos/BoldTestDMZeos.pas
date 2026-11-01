unit BoldTestDMZeos;

{ The database for dmBoldTest under Free Pascal: Zeos with a shared-cache
  in-memory SQLite database, the counterpart of BoldTestDMFireDAC on Delphi.
  Linking this unit (FPC/UnitTestFPC.lpr) registers it as dmBoldTest's
  connector. UnitTest.ini is not read: SQLite is the only engine so far.

  The connection and the adapter are created in code. On Delphi the adapter
  is streamed with SQL Server settings, but the SQLite path switches the
  engine to dbeGenericANSISQL92, which resets them, so both builds end up
  with the same configuration. }

interface

implementation

uses
  SysUtils,
  ZConnection,
  BoldSQLDatabaseConfig,
  BoldDatabaseAdapterZeos,
  dmBoldTest;

const
  { Shared-cache in-memory SQLite: every connection of the process that opens
    the same file: name sees the same database, as CreateAnotherDatabaseConnection
    needs. The name must not carry ?mode=memory&cache=shared: Zeos reads the ?
    part as its own URL parameters and silently opens a file on disk instead. }
  SQLITE_OPEN_READWRITE = $02;
  SQLITE_OPEN_CREATE = $04;
  SQLITE_OPEN_URI = $40;
  SQLITE_OPEN_MEMORY = $80;
  SQLITE_OPEN_SHAREDCACHE = $20000;

function CreateConnection(DataModule: TBoldTestDM): TZConnection;
begin
  // Owned by the test data module, so it goes when BoldTestDM is freed.
  Result := TZConnection.Create(DataModule);
  Result.Protocol := 'sqlite';
  Result.Database := 'file:memdb1';
  Result.Properties.Values['SQLiteOpen_Flags'] := IntToStr(SQLITE_OPEN_READWRITE or
    SQLITE_OPEN_CREATE or SQLITE_OPEN_URI or SQLITE_OPEN_MEMORY or SQLITE_OPEN_SHAREDCACHE);
  Result.LoginPrompt := False;
end;

procedure ConnectZeos(DataModule: TBoldTestDM);
var
  KeepAlive: TZConnection;
  Connection: TZConnection;
  Adapter: TBoldDatabaseAdapterZeos;
begin
  { An in-memory SQLite database exists only while a connection to it is
    open, and Bold closes and reopens its connection between creating the
    schema and activating the system (TBoldSystemSQLMapper.CloseDataBase,
    OpenDatabase). This connection, which runs no statements, keeps the
    database alive until BoldTestDM is freed, so every EnsureBoldTestDM
    still starts from an empty database. }
  KeepAlive := CreateConnection(DataModule);
  KeepAlive.Connect;

  Connection := CreateConnection(DataModule);

  Adapter := TBoldDatabaseAdapterZeos.Create(DataModule);
  Adapter.Connection := Connection;
  Adapter.DatabaseEngine := dbeGenericANSISQL92;
  with Adapter.SQLDatabaseConfig do
  begin
    ColumnTypeForText := 'TEXT';
    ColumnTypeForUnicodeText := 'TEXT';
    ColumnTypeForAnsiText := 'TEXT';
    ColumnTypeForInt64 := 'INTEGER';
  end;
  DataModule.BoldPersistenceHandleDB1.DatabaseAdapter := Adapter;

  WriteLn('BoldTestDM: Using SQLite in-memory through Zeos');
  Connection.Connect;
end;

initialization
  RegisterBoldTestDMConnector(ConnectZeos);

end.
