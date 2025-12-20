{$INCLUDE bold.inc}
unit DemoDataModule;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IniFiles,
  Data.DB,

  // FireDAC
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client,
  FireDAC.Phys.MSSQL,
  FireDAC.Phys.MSSQLDef,
  FireDAC.Phys.PG,
  FireDAC.Phys.PGDef,
  FireDAC.Phys.FB,
  FireDAC.Phys.FBDef,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef,

  // Bold
  BoldAbstractModel,
  BoldAbstractDatabaseAdapter,
  BoldAbstractPersistenceHandleDB,
  BoldDatabaseAdapterFireDAC,
  BoldHandle,
  BoldHandles,
  BoldModel,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldSubscription,
  BoldSystem,
  BoldSystemHandle,
  BoldSQLDatabaseConfig;

type
  TDemoDataModule = class(TDataModule)
    BoldModel1: TBoldModel;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    FDConnection1: TFDConnection;
    BoldDatabaseAdapterFireDAC1: TBoldDatabaseAdapterFireDAC;
    procedure DataModuleCreate(Sender: TObject);
  private
    FConfigFile: string;
    procedure LoadConfiguration;
    procedure ConfigureMSSQL(AIni: TIniFile);
    procedure ConfigurePostgreSQL(AIni: TIniFile);
    procedure ConfigureFirebird(AIni: TIniFile);
    procedure ConfigureSQLite(AIni: TIniFile);
  public
    procedure CreateDatabaseSchema;
    procedure EnsureDatabaseExists(const ADatabaseName: string);
    procedure OpenSystem;
    procedure CloseSystem;
    property ConfigFile: string read FConfigFile write FConfigFile;
  end;

var
  dmDemo: TDemoDataModule;

implementation

{$R *.dfm}

procedure TDemoDataModule.DataModuleCreate(Sender: TObject);
begin
  FConfigFile := ExtractFilePath(ParamStr(0)) + 'DemoDatabase.ini';
  if FileExists(FConfigFile) then
    LoadConfiguration;
end;

procedure TDemoDataModule.LoadConfiguration;
var
  Ini: TIniFile;
  DatabaseType: string;
begin
  Ini := TIniFile.Create(FConfigFile);
  try
    DatabaseType := Ini.ReadString('Database', 'Type', 'MSSQL');

    if SameText(DatabaseType, 'MSSQL') then
      ConfigureMSSQL(Ini)
    else if SameText(DatabaseType, 'PostgreSQL') then
      ConfigurePostgreSQL(Ini)
    else if SameText(DatabaseType, 'Firebird') then
      ConfigureFirebird(Ini)
    else if SameText(DatabaseType, 'SQLite') then
      ConfigureSQLite(Ini);
  finally
    Ini.Free;
  end;
end;

procedure TDemoDataModule.ConfigureMSSQL(AIni: TIniFile);
begin
  FDConnection1.Params.Clear;
  FDConnection1.Params.Add('DriverID=MSSQL');
  FDConnection1.Params.Add('Server=' + AIni.ReadString('MSSQL', 'Server', 'localhost'));
  FDConnection1.Params.Add('Database=' + AIni.ReadString('MSSQL', 'Database', 'BoldDemo'));
  if AIni.ReadBool('MSSQL', 'OSAuthent', True) then
    FDConnection1.Params.Add('OSAuthent=Yes')
  else
  begin
    FDConnection1.Params.Add('User_Name=' + AIni.ReadString('MSSQL', 'User', 'sa'));
    FDConnection1.Params.Add('Password=' + AIni.ReadString('MSSQL', 'Password', ''));
  end;
  BoldDatabaseAdapterFireDAC1.DatabaseEngine := dbeSQLServer;
end;

procedure TDemoDataModule.ConfigurePostgreSQL(AIni: TIniFile);
begin
  FDConnection1.Params.Clear;
  FDConnection1.Params.Add('DriverID=PG');
  FDConnection1.Params.Add('Server=' + AIni.ReadString('PostgreSQL', 'Server', 'localhost'));
  FDConnection1.Params.Add('Database=' + AIni.ReadString('PostgreSQL', 'Database', 'bolddemo'));
  FDConnection1.Params.Add('User_Name=' + AIni.ReadString('PostgreSQL', 'User', 'postgres'));
  FDConnection1.Params.Add('Password=' + AIni.ReadString('PostgreSQL', 'Password', ''));
  BoldDatabaseAdapterFireDAC1.DatabaseEngine := dbePostgres;
end;

procedure TDemoDataModule.ConfigureFirebird(AIni: TIniFile);
begin
  FDConnection1.Params.Clear;
  FDConnection1.Params.Add('DriverID=FB');
  FDConnection1.Params.Add('Database=' + AIni.ReadString('Firebird', 'Database', 'BoldDemo.fdb'));
  FDConnection1.Params.Add('User_Name=' + AIni.ReadString('Firebird', 'User', 'SYSDBA'));
  FDConnection1.Params.Add('Password=' + AIni.ReadString('Firebird', 'Password', 'masterkey'));
  BoldDatabaseAdapterFireDAC1.DatabaseEngine := dbeInterbaseSQLDialect3;
end;

procedure TDemoDataModule.ConfigureSQLite(AIni: TIniFile);
begin
  FDConnection1.Params.Clear;
  FDConnection1.Params.Add('DriverID=SQLite');
  FDConnection1.Params.Add('Database=' + AIni.ReadString('SQLite', 'Database', 'BoldDemo.db'));
  BoldDatabaseAdapterFireDAC1.DatabaseEngine := dbeGenericANSISQL92;
end;

procedure TDemoDataModule.EnsureDatabaseExists(const ADatabaseName: string);
var
  Ini: TIniFile;
  DatabaseType: string;
  TempConn: TFDConnection;
begin
  if not FileExists(FConfigFile) then
    raise Exception.Create('Configuration file not found: ' + FConfigFile);

  Ini := TIniFile.Create(FConfigFile);
  TempConn := TFDConnection.Create(nil);
  try
    DatabaseType := Ini.ReadString('Database', 'Type', 'MSSQL');

    if SameText(DatabaseType, 'MSSQL') then
    begin
      // Create database via master
      TempConn.Params.Clear;
      TempConn.Params.Add('DriverID=MSSQL');
      TempConn.Params.Add('Server=' + Ini.ReadString('MSSQL', 'Server', 'localhost'));
      TempConn.Params.Add('Database=master');
      if Ini.ReadBool('MSSQL', 'OSAuthent', True) then
        TempConn.Params.Add('OSAuthent=Yes')
      else
      begin
        TempConn.Params.Add('User_Name=' + Ini.ReadString('MSSQL', 'User', 'sa'));
        TempConn.Params.Add('Password=' + Ini.ReadString('MSSQL', 'Password', ''));
      end;
      TempConn.Connected := True;
      TempConn.ExecSQL('IF NOT EXISTS (SELECT name FROM sys.databases WHERE name = ' + QuotedStr(ADatabaseName) + ') CREATE DATABASE [' + ADatabaseName + ']');
      TempConn.Connected := False;
      // Update main connection to use new database
      FDConnection1.Params.Values['Database'] := ADatabaseName;
    end
    else if SameText(DatabaseType, 'PostgreSQL') then
    begin
      TempConn.Params.Clear;
      TempConn.Params.Add('DriverID=PG');
      TempConn.Params.Add('Server=' + Ini.ReadString('PostgreSQL', 'Server', 'localhost'));
      TempConn.Params.Add('Database=postgres');
      TempConn.Params.Add('User_Name=' + Ini.ReadString('PostgreSQL', 'User', 'postgres'));
      TempConn.Params.Add('Password=' + Ini.ReadString('PostgreSQL', 'Password', ''));
      TempConn.Connected := True;
      TempConn.ExecSQL('CREATE DATABASE ' + ADatabaseName, True);
      TempConn.Connected := False;
      FDConnection1.Params.Values['Database'] := ADatabaseName;
    end
    else if SameText(DatabaseType, 'Firebird') then
    begin
      if not FileExists(ADatabaseName) then
      begin
        TempConn.Params.Clear;
        TempConn.Params.Add('DriverID=FB');
        TempConn.Params.Add('Database=' + ADatabaseName);
        TempConn.Params.Add('User_Name=' + Ini.ReadString('Firebird', 'User', 'SYSDBA'));
        TempConn.Params.Add('Password=' + Ini.ReadString('Firebird', 'Password', 'masterkey'));
        TempConn.Params.Add('CreateDatabase=Yes');
        TempConn.Connected := True;
        TempConn.Connected := False;
      end;
      FDConnection1.Params.Values['Database'] := ADatabaseName;
    end
    else if SameText(DatabaseType, 'SQLite') then
    begin
      // SQLite creates the database file automatically when connecting
      FDConnection1.Params.Values['Database'] := ADatabaseName;
    end;
  finally
    TempConn.Free;
    Ini.Free;
  end;
end;

procedure TDemoDataModule.CreateDatabaseSchema;
begin
  BoldPersistenceHandleDB1.CreateDataBaseSchema;
end;

procedure TDemoDataModule.OpenSystem;
begin
  BoldSystemHandle1.Active := True;
end;

procedure TDemoDataModule.CloseSystem;
begin
  if BoldSystemHandle1.Active then
  begin
    if BoldSystemHandle1.System.DirtyObjects.Count > 0 then
      BoldSystemHandle1.System.UpdateDatabase;
    BoldSystemHandle1.Active := False;
  end;
end;

end.
