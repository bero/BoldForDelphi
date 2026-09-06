unit BoldTestDatabaseConfig;

{******************************************************************************}
{                                                                              }
{  BoldTestDatabaseConfig - Shared database configuration for unit tests       }
{                                                                              }
{  Reads database connection settings from UnitTest.ini and configures         }
{  FireDAC connections for various database engines (SQL Server, SQLite,       }
{  Interbase, Firebird, PostgreSQL).                                           }
{                                                                              }
{  The engine can be overridden without editing the ini through the            }
{  environment variable BOLD_TEST_ENGINE, so a script can run the same exe     }
{  against several engines in turn (see UnitTest\run_matrix.ps1).             }
{                                                                              }
{******************************************************************************}

interface

uses
  BoldDatabaseAdapterFireDAC,
  FireDAC.Comp.Client;

procedure ConfigureConnection(Connection: TFDConnection; Adapter: TBoldDatabaseAdapterFireDAC);
{ Creates the test database on server engines if it does not exist. With a
  name, creates that database instead of the one in UnitTest.ini - for tests
  that need a second database next to the shared one (e.g. a copy target). }
procedure CreateTestDatabase(const ADatabaseName: string = '');
procedure DropTestDatabase;
{ The engine to test against: the BOLD_TEST_ENGINE environment variable when
  set, otherwise [Database] Engine in UnitTest.ini. Every reader of the engine
  goes through here so the override is honoured everywhere. }
function GetTestDatabaseEngine: string; overload;
function GetTestDatabaseEngine(const AIniPath: string): string; overload;
{ The database name from UnitTest.ini for the configured engine. }
function GetTestDatabaseName: string;
function GetIniFilePath: string;

implementation

uses
  System.SysUtils,
  System.IniFiles,
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

function GetIniFilePath: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'UnitTest.ini';
  if not FileExists(Result) then
    Result := ExtractFilePath(ParamStr(0)) + '..\UnitTest.ini';
  if not FileExists(Result) then
    raise Exception.Create('UnitTest.ini not found. Expected at: ' + Result);
end;

const
  cTestEngineEnvVar = 'BOLD_TEST_ENGINE';

function GetTestDatabaseEngine(const AIniPath: string): string;
var
  Ini: TIniFile;
begin
  Result := Trim(GetEnvironmentVariable(cTestEngineEnvVar));
  if Result <> '' then
    Exit;
  Ini := TIniFile.Create(AIniPath);
  try
    Result := Ini.ReadString('Database', 'Engine', 'SQLServer');
  finally
    Ini.Free;
  end;
end;

function GetTestDatabaseEngine: string;
begin
  Result := GetTestDatabaseEngine(GetIniFilePath);
end;

procedure ConfigureConnection(Connection: TFDConnection; Adapter: TBoldDatabaseAdapterFireDAC);
var
  Ini: TIniFile;
  Engine: string;
  Server, Database, User, Password: string;
  Port: Integer;
  OSAuth: Boolean;
begin
  Ini := TIniFile.Create(GetIniFilePath);
  try
    Engine := GetTestDatabaseEngine;

    Connection.Params.Clear;

    if SameText(Engine, 'SQLServer') then
    begin
      Server := Ini.ReadString('SQLServer', 'Server', '.\SQLEXPRESS');
      Database := Ini.ReadString('SQLServer', 'Database', 'BoldUnitTest');
      User := Ini.ReadString('SQLServer', 'User', '');
      Password := Ini.ReadString('SQLServer', 'Password', '');
      // Handle Yes/No/True/False/1/0
      OSAuth := SameText(Ini.ReadString('SQLServer', 'OSAuthentication', 'Yes'), 'Yes') or
                SameText(Ini.ReadString('SQLServer', 'OSAuthentication', 'Yes'), 'True') or
                SameText(Ini.ReadString('SQLServer', 'OSAuthentication', 'Yes'), '1');

      Connection.DriverName := 'MSSQL';
      Connection.Params.Values['Server'] := Server;
      Connection.Params.Values['Database'] := Database;
      if OSAuth then
        Connection.Params.Values['OSAuthent'] := 'Yes'
      else
      begin
        Connection.Params.Values['User_Name'] := User;
        Connection.Params.Values['Password'] := Password;
      end;

      Adapter.DatabaseEngine := dbeSQLServer;
    end
    else if SameText(Engine, 'SQLite') then
    begin
      Database := Ini.ReadString('SQLite', 'Database', 'file:memdb1?mode=memory&cache=shared');
      Connection.DriverName := 'SQLite';
      Connection.Params.Values['Database'] := Database;
      Adapter.DatabaseEngine := dbeGenericANSISQL92;
      with Adapter.SQLDatabaseConfig do
      begin
        ColumnTypeForText := 'TEXT';
        ColumnTypeForUnicodeText := 'TEXT';
        ColumnTypeForAnsiText := 'TEXT';
        ColumnTypeForInt64 := 'INTEGER';
      end;
    end
    else if SameText(Engine, 'Interbase') then
    begin
      Server := Ini.ReadString('Interbase', 'Server', 'localhost');
      Database := Ini.ReadString('Interbase', 'Database', '');
      User := Ini.ReadString('Interbase', 'User', 'SYSDBA');
      Password := Ini.ReadString('Interbase', 'Password', 'masterkey');

      Connection.DriverName := 'IB';
      Connection.Params.Values['Server'] := Server;
      Connection.Params.Values['Database'] := Database;
      Connection.Params.Values['User_Name'] := User;
      Connection.Params.Values['Password'] := Password;

      Adapter.DatabaseEngine := dbeInterbaseSQLDialect3;
    end
    else if SameText(Engine, 'Firebird') then
    begin
      Server := Ini.ReadString('Firebird', 'Server', 'localhost');
      Database := Ini.ReadString('Firebird', 'Database', '');
      User := Ini.ReadString('Firebird', 'User', 'SYSDBA');
      Password := Ini.ReadString('Firebird', 'Password', 'masterkey');

      Connection.DriverName := 'FB';
      Connection.Params.Values['Server'] := Server;
      Connection.Params.Values['Database'] := Database;
      Connection.Params.Values['User_Name'] := User;
      Connection.Params.Values['Password'] := Password;

      Adapter.DatabaseEngine := dbeInterbaseSQLDialect3;
    end
    else if SameText(Engine, 'PostgreSQL') then
    begin
      Server := Ini.ReadString('PostgreSQL', 'Server', 'localhost');
      Port := Ini.ReadInteger('PostgreSQL', 'Port', 5432);
      Database := Ini.ReadString('PostgreSQL', 'Database', 'boldunittest');
      User := Ini.ReadString('PostgreSQL', 'User', 'postgres');
      Password := Ini.ReadString('PostgreSQL', 'Password', '');

      Connection.DriverName := 'PG';
      Connection.Params.Values['Server'] := Server;
      Connection.Params.Values['Port'] := IntToStr(Port);
      Connection.Params.Values['Database'] := Database;
      Connection.Params.Values['User_Name'] := User;
      Connection.Params.Values['Password'] := Password;

      Adapter.DatabaseEngine := dbePostgres;
    end
    else
      raise Exception.CreateFmt('Unknown database engine: %s', [Engine]);

    Connection.LoginPrompt := False;
  finally
    Ini.Free;
  end;
end;

function GetTestDatabaseName: string;
var
  Ini: TIniFile;
  Engine: string;
begin
  Ini := TIniFile.Create(GetIniFilePath);
  try
    Engine := GetTestDatabaseEngine;
    if SameText(Engine, 'SQLite') then
      Result := Ini.ReadString('SQLite', 'Database', 'file:memdb1?mode=memory&cache=shared')
    else
      Result := Ini.ReadString(Engine, 'Database', 'BoldUnitTest');
  finally
    Ini.Free;
  end;
end;

procedure CreateTestDatabase(const ADatabaseName: string);
var
  Ini: TIniFile;
  Engine, Server, Database, User, Password: string;
  OSAuth: Boolean;
  TempConn: TFDConnection;
begin
  Ini := TIniFile.Create(GetIniFilePath);
  TempConn := TFDConnection.Create(nil);
  try
    Engine := GetTestDatabaseEngine;

    if SameText(Engine, 'SQLServer') then
    begin
      Server := Ini.ReadString('SQLServer', 'Server', '.\SQLEXPRESS');
      if ADatabaseName <> '' then
        Database := ADatabaseName
      else
        Database := Ini.ReadString('SQLServer', 'Database', 'BoldUnitTest');
      User := Ini.ReadString('SQLServer', 'User', '');
      Password := Ini.ReadString('SQLServer', 'Password', '');
      // Handle Yes/No/True/False/1/0
      OSAuth := SameText(Ini.ReadString('SQLServer', 'OSAuthentication', 'Yes'), 'Yes') or
                SameText(Ini.ReadString('SQLServer', 'OSAuthentication', 'Yes'), 'True') or
                SameText(Ini.ReadString('SQLServer', 'OSAuthentication', 'Yes'), '1');

      // Connect to master database to create test database
      TempConn.DriverName := 'MSSQL';
      TempConn.Params.Values['Server'] := Server;
      TempConn.Params.Values['Database'] := 'master';
      if OSAuth then
        TempConn.Params.Values['OSAuthent'] := 'Yes'
      else
      begin
        TempConn.Params.Values['User_Name'] := User;
        TempConn.Params.Values['Password'] := Password;
      end;
      TempConn.LoginPrompt := False;
      TempConn.Open;

      // Create database if it doesn't exist
      TempConn.ExecSQL('IF NOT EXISTS (SELECT * FROM sys.databases WHERE name = ''' + Database + ''') CREATE DATABASE [' + Database + ']');
      TempConn.Close;
    end;
    // For other engines (SQLite, Firebird, etc.), database is created automatically or handled differently
  finally
    TempConn.Free;
    Ini.Free;
  end;
end;

procedure DropTestDatabase;
var
  Ini: TIniFile;
  Engine, Server, Database, User, Password: string;
  OSAuth: Boolean;
  TempConn: TFDConnection;
begin
  Ini := TIniFile.Create(GetIniFilePath);
  TempConn := TFDConnection.Create(nil);
  try
    Engine := GetTestDatabaseEngine;

    if SameText(Engine, 'SQLServer') then
    begin
      Server := Ini.ReadString('SQLServer', 'Server', '.\SQLEXPRESS');
      Database := Ini.ReadString('SQLServer', 'Database', 'BoldUnitTest');
      User := Ini.ReadString('SQLServer', 'User', '');
      Password := Ini.ReadString('SQLServer', 'Password', '');
      // Handle Yes/No/True/False/1/0
      OSAuth := SameText(Ini.ReadString('SQLServer', 'OSAuthentication', 'Yes'), 'Yes') or
                SameText(Ini.ReadString('SQLServer', 'OSAuthentication', 'Yes'), 'True') or
                SameText(Ini.ReadString('SQLServer', 'OSAuthentication', 'Yes'), '1');

      // Connect to master database to drop test database
      TempConn.DriverName := 'MSSQL';
      TempConn.Params.Values['Server'] := Server;
      TempConn.Params.Values['Database'] := 'master';
      if OSAuth then
        TempConn.Params.Values['OSAuthent'] := 'Yes'
      else
      begin
        TempConn.Params.Values['User_Name'] := User;
        TempConn.Params.Values['Password'] := Password;
      end;
      TempConn.LoginPrompt := False;
      TempConn.Open;

      // Drop database if it exists
      TempConn.ExecSQL('IF EXISTS (SELECT * FROM sys.databases WHERE name = ''' + Database + ''') BEGIN ALTER DATABASE [' + Database + '] SET SINGLE_USER WITH ROLLBACK IMMEDIATE; DROP DATABASE [' + Database + ']; END');
      TempConn.Close;
    end
    else if SameText(Engine, 'SQLite') then
    begin
      Database := Ini.ReadString('SQLite', 'Database', 'unittest.db');
      if (Database <> ':memory:') and FileExists(Database) then
        DeleteFile(Database);
    end;
    // For Firebird/Interbase, manual cleanup may be needed
  finally
    TempConn.Free;
    Ini.Free;
  end;
end;

end.
