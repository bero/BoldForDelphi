unit BoldTestZeos;

{ Zeos connections and database adapters for the Free Pascal test data
  modules (BoldTestDMZeos, maan_UndoRedoZeos): a shared-cache in-memory SQLite
  database, configured like the SQLite path of the Delphi test modules. }

interface

uses
  Classes,
  ZConnection,
  BoldDatabaseAdapterZeos;

const
  { The shared in-memory database of the test data modules. }
  cTestZeosDatabase = 'file:memdb1';

{ A connection, owned by Owner, to the shared in-memory database Database
  (memdb1 by default). Every connection of the process to the same name sees
  the same database, as CreateAnotherDatabaseConnection and the second system
  of dmUndoRedo need. Not connected yet. }
function NewTestZeosConnection(Owner: TComponent; const Database: string = cTestZeosDatabase): TZConnection;

{ An adapter, owned by Owner, on Connection: dbeGenericANSISQL92 with the
  column types the Delphi test modules use for SQLite. Switching to that engine
  resets every SQL setting, so both builds end up with the same configuration. }
function NewTestZeosAdapter(Owner: TComponent; Connection: TZConnection): TBoldDatabaseAdapterZeos;

{ An in-memory SQLite database exists only while a connection to it is open,
  and Bold closes and reopens its connection between creating the schema and
  activating the system (TBoldSystemSQLMapper.CloseDataBase, OpenDatabase).
  Returns an opened connection, owned by Owner, that runs no statements and
  keeps the database alive until Owner is freed. }
function NewTestZeosKeepAlive(Owner: TComponent; const Database: string = cTestZeosDatabase): TZConnection;

implementation

uses
  SysUtils,
  BoldSQLDatabaseConfig;

const
  { The name must not carry ?mode=memory&cache=shared: Zeos reads the ? part
    as its own URL parameters and silently opens a file on disk instead. }
  SQLITE_OPEN_READWRITE = $02;
  SQLITE_OPEN_CREATE = $04;
  SQLITE_OPEN_URI = $40;
  SQLITE_OPEN_MEMORY = $80;
  SQLITE_OPEN_SHAREDCACHE = $20000;

function NewTestZeosConnection(Owner: TComponent; const Database: string): TZConnection;
begin
  Result := TZConnection.Create(Owner);
  Result.Protocol := 'sqlite';
  Result.Database := Database;
  Result.Properties.Values['SQLiteOpen_Flags'] := IntToStr(SQLITE_OPEN_READWRITE or
    SQLITE_OPEN_CREATE or SQLITE_OPEN_URI or SQLITE_OPEN_MEMORY or SQLITE_OPEN_SHAREDCACHE);
  Result.LoginPrompt := False;
end;

function NewTestZeosAdapter(Owner: TComponent; Connection: TZConnection): TBoldDatabaseAdapterZeos;
begin
  Result := TBoldDatabaseAdapterZeos.Create(Owner);
  Result.Connection := Connection;
  Result.DatabaseEngine := dbeGenericANSISQL92;
  with Result.SQLDatabaseConfig do
  begin
    ColumnTypeForText := 'TEXT';
    ColumnTypeForUnicodeText := 'TEXT';
    ColumnTypeForAnsiText := 'TEXT';
    ColumnTypeForInt64 := 'INTEGER';
  end;
end;

function NewTestZeosKeepAlive(Owner: TComponent; const Database: string): TZConnection;
begin
  Result := NewTestZeosConnection(Owner, Database);
  Result.Connect;
end;

end.
