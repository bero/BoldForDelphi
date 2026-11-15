unit BoldTestZeos;

{ Zeos connections and database adapters for the Free Pascal test data
  modules (BoldTestDMZeos, maan_UndoRedoZeos) and test bases.

  The engine is chosen as on Delphi (BoldTestDatabaseConfig): the
  BOLD_TEST_ENGINE environment variable when set, otherwise [Database] Engine
  in UnitTest.ini. Two engines are set up so far:
  - SQLite: a shared-cache in-memory database, configured like the SQLite path
    of the Delphi test modules.
  - PostgreSQL: the server of [PostgreSQL] in UnitTest.ini. The environment
    variable BOLD_TEST_PG_PORT overrides its port, for a server that does not
    listen where the ini says. The databases are created when missing. }

interface

uses
  Classes,
  ZConnection,
  BoldDatabaseAdapterZeos;

const
  { The shared database of the test data modules: in memory on SQLite, the
    database of UnitTest.ini on PostgreSQL. }
  cTestZeosDatabase = 'file:memdb1';

{ True when the tests run on PostgreSQL, False on SQLite. Raises for an engine
  that is not set up for Zeos. }
function TestZeosEngineIsPostgres: Boolean;

{ What the tests run on, for the log. }
function TestZeosEngineDescription: string;

{ A connection, owned by Owner, to the test database Database (the shared one
  by default) on the engine of the run. Every connection of the process to the
  same name sees the same database, as CreateAnotherDatabaseConnection and the
  second system of dmUndoRedo need. On PostgreSQL another name than the shared
  one is a database of its own next to it, <database>_<name>. Not connected
  yet. }
function NewTestZeosConnection(Owner: TComponent; const Database: string = cTestZeosDatabase): TZConnection;

{ The same on SQLite whatever the engine of the run, for tests that need a
  database of their own that is gone afterwards. }
function NewTestZeosSQLiteConnection(Owner: TComponent; const Database: string): TZConnection;

{ An adapter, owned by Owner, on Connection, for the engine Connection is set
  up for. SQLite: dbeGenericANSISQL92 with the column types the Delphi test
  modules use for SQLite; switching to that engine resets every SQL setting,
  so both builds end up with the same configuration. PostgreSQL: dbePostgres. }
function NewTestZeosAdapter(Owner: TComponent; Connection: TZConnection): TBoldDatabaseAdapterZeos;

{ An in-memory SQLite database exists only while a connection to it is open,
  and Bold closes and reopens its connection between creating the schema and
  activating the system (TBoldSystemSQLMapper.CloseDataBase, OpenDatabase).
  Returns an opened connection, owned by Owner, that runs no statements and
  keeps the database alive until Owner is freed. A PostgreSQL database stays
  by itself: there the connection is returned unopened. }
function NewTestZeosKeepAlive(Owner: TComponent; const Database: string = cTestZeosDatabase): TZConnection;

implementation

uses
  SysUtils,
  IniFiles,
  ZDbcIntfs,
  BoldSQLDatabaseConfig;

const
  { The name must not carry ?mode=memory&cache=shared: Zeos reads the ? part
    as its own URL parameters and silently opens a file on disk instead. }
  SQLITE_OPEN_READWRITE = $02;
  SQLITE_OPEN_CREATE = $04;
  SQLITE_OPEN_URI = $40;
  SQLITE_OPEN_MEMORY = $80;
  SQLITE_OPEN_SHAREDCACHE = $20000;

  cSQLiteProtocol = 'sqlite';
  cPostgresProtocol = 'postgresql';
  cPostgresSection = 'PostgreSQL';
  cFilePrefix = 'file:';

var
  // The PostgreSQL databases this run has seen or created
  KnownPostgresDatabases: TStringList;

{ UnitTest.ini is in UnitTest, the runner in UnitTest/FPC/bin or bin-heaptrc.
  Empty when there is none: the defaults apply then. }
function TestIniFileName: string;
var
  Dir: string;
  i: Integer;
begin
  Dir := ExtractFilePath(ParamStr(0));
  for i := 0 to 2 do
  begin
    Result := Dir + 'UnitTest.ini';
    if FileExists(Result) then
      Exit;
    Dir := Dir + '..' + PathDelim;
  end;
  Result := '';
end;

function ReadTestSetting(const Section, Key, Default: string): string;
var
  FileName: string;
  Ini: TIniFile;
begin
  Result := Default;
  FileName := TestIniFileName;
  if FileName = '' then
    Exit;
  Ini := TIniFile.Create(FileName);
  try
    Result := Trim(Ini.ReadString(Section, Key, Default));
  finally
    Ini.Free;
  end;
end;

function TestEnvironmentVariable(const Name: string): string;
begin
  Result := Trim(GetEnvironmentVariable(Name));
end;

function TestZeosEngineIsPostgres: Boolean;
var
  Engine: string;
begin
  Engine := TestEnvironmentVariable('BOLD_TEST_ENGINE');
  if Engine = '' then
    Engine := ReadTestSetting('Database', 'Engine', 'SQLite');
  Result := SameText(Engine, 'PostgreSQL');
  if not Result and not SameText(Engine, 'SQLite') then
    raise Exception.CreateFmt('The Zeos tests are not set up for the database engine %s: ' +
      'SQLite and PostgreSQL are (BOLD_TEST_ENGINE, or [Database] Engine in UnitTest.ini)', [Engine]);
end;

function PostgresServer: string;
begin
  Result := ReadTestSetting(cPostgresSection, 'Server', 'localhost');
end;

function PostgresPort: Integer;
var
  Port: string;
begin
  Port := TestEnvironmentVariable('BOLD_TEST_PG_PORT');
  if Port = '' then
    Port := ReadTestSetting(cPostgresSection, 'Port', '5432');
  Result := StrToInt(Port);
end;

{ The PostgreSQL database for the test database Database: the one of
  UnitTest.ini for the shared database, and for another name that one with the
  name behind it. PostgreSQL folds unquoted names to lower case. }
function PostgresDatabaseName(const Database: string): string;
begin
  Result := LowerCase(ReadTestSetting(cPostgresSection, 'Database', 'boldunittest'));
  if Database = cTestZeosDatabase then
    Exit;
  if Pos(cFilePrefix, Database) = 1 then
    Result := Result + '_' + LowerCase(Copy(Database, Length(cFilePrefix) + 1, MaxInt))
  else
    Result := Result + '_' + LowerCase(Database);
end;

function NewPostgresConnection(Owner: TComponent; const PostgresDatabase: string): TZConnection;
begin
  Result := TZConnection.Create(Owner);
  Result.Protocol := cPostgresProtocol;
  Result.HostName := PostgresServer;
  Result.Port := PostgresPort;
  Result.User := ReadTestSetting(cPostgresSection, 'User', 'postgres');
  Result.Password := ReadTestSetting(cPostgresSection, 'Password', '');
  Result.Database := PostgresDatabase;
  Result.LoginPrompt := False;
end;

{ Creates the database when the server does not have it, once per run. }
procedure EnsurePostgresDatabase(const PostgresDatabase: string);
var
  Maintenance: TZConnection;
  Exists: Boolean;
begin
  if not Assigned(KnownPostgresDatabases) then
    KnownPostgresDatabases := TStringList.Create;
  if KnownPostgresDatabases.IndexOf(PostgresDatabase) >= 0 then
    Exit;
  // Every server has the database postgres
  Maintenance := NewPostgresConnection(nil, 'postgres');
  try
    Maintenance.Connect;
    Exists := Maintenance.DbcConnection.CreateStatement.ExecuteQuery(
      'SELECT datname FROM pg_catalog.pg_database WHERE datname = ''' + PostgresDatabase + '''').Next;
    if not Exists then
      Maintenance.ExecuteDirect('CREATE DATABASE ' + PostgresDatabase);
  finally
    Maintenance.Free;
  end;
  KnownPostgresDatabases.Add(PostgresDatabase);
end;

function TestZeosEngineDescription: string;
begin
  if TestZeosEngineIsPostgres then
    Result := Format('PostgreSQL through Zeos (%s:%d, database %s)',
      [PostgresServer, PostgresPort, PostgresDatabaseName(cTestZeosDatabase)])
  else
    Result := 'SQLite in-memory through Zeos';
end;

function NewTestZeosSQLiteConnection(Owner: TComponent; const Database: string): TZConnection;
begin
  Result := TZConnection.Create(Owner);
  Result.Protocol := cSQLiteProtocol;
  Result.Database := Database;
  Result.Properties.Values['SQLiteOpen_Flags'] := IntToStr(SQLITE_OPEN_READWRITE or
    SQLITE_OPEN_CREATE or SQLITE_OPEN_URI or SQLITE_OPEN_MEMORY or SQLITE_OPEN_SHAREDCACHE);
  Result.LoginPrompt := False;
end;

function NewTestZeosConnection(Owner: TComponent; const Database: string): TZConnection;
var
  PostgresDatabase: string;
begin
  if TestZeosEngineIsPostgres then
  begin
    PostgresDatabase := PostgresDatabaseName(Database);
    EnsurePostgresDatabase(PostgresDatabase);
    Result := NewPostgresConnection(Owner, PostgresDatabase);
  end
  else
    Result := NewTestZeosSQLiteConnection(Owner, Database);
end;

function NewTestZeosAdapter(Owner: TComponent; Connection: TZConnection): TBoldDatabaseAdapterZeos;
begin
  Result := TBoldDatabaseAdapterZeos.Create(Owner);
  Result.Connection := Connection;
  if SameText(string(Connection.Protocol), cPostgresProtocol) then
    Result.DatabaseEngine := dbePostgres
  else
  begin
    Result.DatabaseEngine := dbeGenericANSISQL92;
    with Result.SQLDatabaseConfig do
    begin
      ColumnTypeForText := 'TEXT';
      ColumnTypeForUnicodeText := 'TEXT';
      ColumnTypeForAnsiText := 'TEXT';
      ColumnTypeForInt64 := 'INTEGER';
    end;
  end;
end;

function NewTestZeosKeepAlive(Owner: TComponent; const Database: string): TZConnection;
begin
  Result := NewTestZeosConnection(Owner, Database);
  if not TestZeosEngineIsPostgres then
    Result.Connect;
end;

initialization

finalization
  FreeAndNil(KnownPostgresDatabases);

end.
