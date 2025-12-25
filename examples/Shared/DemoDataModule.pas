{$INCLUDE bold.inc}
unit DemoDataModule;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IniFiles,
  System.IOUtils,
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

  {$IFDEF UNIDAC}
  // UniDAC (optional - define UNIDAC in project options if available)
  Uni,
  BoldDatabaseAdapterUniDAC,
  {$ENDIF}

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
  BoldPersistenceHandleFileXML,
  BoldSubscription,
  BoldSystem,
  BoldSystemHandle,
  BoldSQLDatabaseConfig;

type
  TPersistenceType = (ptFireDAC, {$IFDEF UNIDAC}ptUniDAC,{$ENDIF} ptXML);

  TDemoDataModule = class(TDataModule)
    BoldModel1: TBoldModel;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FConfigFile: string;
    FPersistenceType: TPersistenceType;

    // Database persistence components
    FPersistenceHandleDB: TBoldPersistenceHandleDB;
    FFDConnection: TFDConnection;
    FFireDACAdapter: TBoldDatabaseAdapterFireDAC;
    {$IFDEF UNIDAC}
    FUniConnection: TUniConnection;
    FUniDACAdapter: TBoldDatabaseAdapterUniDAC;
    {$ENDIF}

    // XML persistence components
    FPersistenceHandleXML: TBoldPersistenceHandleFileXML;
    FXMLFileName: string;

    procedure LoadConfiguration;
    procedure CreatePersistenceHandle;
    procedure ConfigureMSSQL(AIni: TIniFile);
    procedure ConfigurePostgreSQL(AIni: TIniFile);
    procedure ConfigureFirebird(AIni: TIniFile);
    procedure ConfigureSQLite(AIni: TIniFile);
    procedure ConfigureXML(AIni: TIniFile);
    procedure ConfigureSQLDatabaseConfig(AConfig: TBoldSQLDatabaseConfig; const ADatabaseType: string);
  public
    procedure CreateDatabaseSchema;
    procedure EnsureDatabaseExists(const ADatabaseName: string);
    procedure OpenSystem;
    procedure CloseSystem;
    function GetConnected: Boolean;
    function GetDatabaseName: string;
    property ConfigFile: string read FConfigFile write FConfigFile;
    property PersistenceType: TPersistenceType read FPersistenceType;
    property Connected: Boolean read GetConnected;
    property DatabaseName: string read GetDatabaseName;
  end;

var
  dmDemo: TDemoDataModule;

implementation

{$R *.dfm}

procedure TDemoDataModule.DataModuleCreate(Sender: TObject);
begin
  // INI file shares name with executable: MasterDetail.exe reads MasterDetail.ini
  FConfigFile := ChangeFileExt(ParamStr(0), '.ini');
  if FileExists(FConfigFile) then
    LoadConfiguration;
end;

procedure TDemoDataModule.DataModuleDestroy(Sender: TObject);
begin
  // Free dynamically created components
  FreeAndNil(FFireDACAdapter);
  FreeAndNil(FFDConnection);
  {$IFDEF UNIDAC}
  FreeAndNil(FUniDACAdapter);
  FreeAndNil(FUniConnection);
  {$ENDIF}
  FreeAndNil(FPersistenceHandleDB);
  FreeAndNil(FPersistenceHandleXML);
end;

procedure TDemoDataModule.CreatePersistenceHandle;
begin
  case FPersistenceType of
    ptFireDAC:
      begin
        // Create DB persistence handle
        FPersistenceHandleDB := TBoldPersistenceHandleDB.Create(Self);
        FPersistenceHandleDB.BoldModel := BoldModel1;

        // Create FireDAC connection and adapter
        FFDConnection := TFDConnection.Create(Self);
        FFDConnection.LoginPrompt := False;
        FFireDACAdapter := TBoldDatabaseAdapterFireDAC.Create(Self);
        FFireDACAdapter.Connection := FFDConnection;
        FPersistenceHandleDB.DatabaseAdapter := FFireDACAdapter;

        // Link to system handle
        BoldSystemHandle1.PersistenceHandle := FPersistenceHandleDB;
      end;

    {$IFDEF UNIDAC}
    ptUniDAC:
      begin
        // Create DB persistence handle
        FPersistenceHandleDB := TBoldPersistenceHandleDB.Create(Self);
        FPersistenceHandleDB.BoldModel := BoldModel1;

        // Create UniDAC connection and adapter
        FUniConnection := TUniConnection.Create(Self);
        FUniConnection.LoginPrompt := False;
        FUniDACAdapter := TBoldDatabaseAdapterUniDAC.Create(Self);
        FUniDACAdapter.Connection := FUniConnection;
        FPersistenceHandleDB.DatabaseAdapter := FUniDACAdapter;

        // Link to system handle
        BoldSystemHandle1.PersistenceHandle := FPersistenceHandleDB;
      end;
    {$ENDIF}

    ptXML:
      begin
        // Create XML persistence handle
        FPersistenceHandleXML := TBoldPersistenceHandleFileXML.Create(Self);
        FPersistenceHandleXML.BoldModel := BoldModel1;

        // Link to system handle
        BoldSystemHandle1.PersistenceHandle := FPersistenceHandleXML;
      end;
  end;
end;

procedure TDemoDataModule.ConfigureSQLDatabaseConfig(AConfig: TBoldSQLDatabaseConfig; const ADatabaseType: string);
begin
  if SameText(ADatabaseType, 'MSSQL') then
  begin
    AConfig.ColumnTypeForDate := 'DATE';
    AConfig.ColumnTypeForTime := 'TIME';
    AConfig.ColumnTypeForDateTime := 'DATETIME';
    AConfig.ColumnTypeForBlob := 'VARBINARY(MAX)';
    AConfig.ColumnTypeForFloat := 'FLOAT';
    AConfig.ColumnTypeForCurrency := 'DECIMAL(18,4)';
    AConfig.ColumnTypeForString := 'NVARCHAR(%d)';
    AConfig.ColumnTypeForUnicodeString := 'NVARCHAR(%d)';
    AConfig.ColumnTypeForAnsiString := 'VARCHAR(%d)';
    AConfig.ColumnTypeForText := 'NVARCHAR(MAX)';
    AConfig.ColumnTypeForUnicodeText := 'NVARCHAR(MAX)';
    AConfig.ColumnTypeForAnsiText := 'VARCHAR(MAX)';
    AConfig.ColumnTypeForInteger := 'INT';
    AConfig.ColumnTypeForSmallInt := 'SMALLINT';
    AConfig.ColumnTypeForInt64 := 'BIGINT';
  end
  else if SameText(ADatabaseType, 'PostgreSQL') then
  begin
    AConfig.ColumnTypeForDate := 'DATE';
    AConfig.ColumnTypeForTime := 'TIME';
    AConfig.ColumnTypeForDateTime := 'TIMESTAMP';
    AConfig.ColumnTypeForBlob := 'BYTEA';
    AConfig.ColumnTypeForFloat := 'DOUBLE PRECISION';
    AConfig.ColumnTypeForCurrency := 'DECIMAL(18,4)';
    AConfig.ColumnTypeForString := 'VARCHAR(%d)';
    AConfig.ColumnTypeForUnicodeString := 'VARCHAR(%d)';
    AConfig.ColumnTypeForAnsiString := 'VARCHAR(%d)';
    AConfig.ColumnTypeForText := 'TEXT';
    AConfig.ColumnTypeForUnicodeText := 'TEXT';
    AConfig.ColumnTypeForAnsiText := 'TEXT';
    AConfig.ColumnTypeForInteger := 'INTEGER';
    AConfig.ColumnTypeForSmallInt := 'SMALLINT';
    AConfig.ColumnTypeForInt64 := 'BIGINT';
  end
  else if SameText(ADatabaseType, 'Firebird') then
  begin
    AConfig.ColumnTypeForDate := 'DATE';
    AConfig.ColumnTypeForTime := 'TIME';
    AConfig.ColumnTypeForDateTime := 'TIMESTAMP';
    AConfig.ColumnTypeForBlob := 'BLOB';
    AConfig.ColumnTypeForFloat := 'DOUBLE PRECISION';
    AConfig.ColumnTypeForCurrency := 'DECIMAL(18,4)';
    AConfig.ColumnTypeForString := 'VARCHAR(%d)';
    AConfig.ColumnTypeForUnicodeString := 'VARCHAR(%d)';
    AConfig.ColumnTypeForAnsiString := 'VARCHAR(%d)';
    AConfig.ColumnTypeForText := 'BLOB SUB_TYPE TEXT';
    AConfig.ColumnTypeForUnicodeText := 'BLOB SUB_TYPE TEXT';
    AConfig.ColumnTypeForAnsiText := 'BLOB SUB_TYPE TEXT';
    AConfig.ColumnTypeForInteger := 'INTEGER';
    AConfig.ColumnTypeForSmallInt := 'SMALLINT';
    AConfig.ColumnTypeForInt64 := 'BIGINT';
  end
  else if SameText(ADatabaseType, 'SQLite') then
  begin
    AConfig.ColumnTypeForDate := 'TEXT';
    AConfig.ColumnTypeForTime := 'TEXT';
    AConfig.ColumnTypeForDateTime := 'TEXT';
    AConfig.ColumnTypeForBlob := 'BLOB';
    AConfig.ColumnTypeForFloat := 'REAL';
    AConfig.ColumnTypeForCurrency := 'REAL';
    AConfig.ColumnTypeForString := 'TEXT';
    AConfig.ColumnTypeForUnicodeString := 'TEXT';
    AConfig.ColumnTypeForAnsiString := 'TEXT';
    AConfig.ColumnTypeForText := 'TEXT';
    AConfig.ColumnTypeForUnicodeText := 'TEXT';
    AConfig.ColumnTypeForAnsiText := 'TEXT';
    AConfig.ColumnTypeForInteger := 'INTEGER';
    AConfig.ColumnTypeForSmallInt := 'INTEGER';
    AConfig.ColumnTypeForInt64 := 'INTEGER';
  end;
end;

procedure TDemoDataModule.LoadConfiguration;
var
  Ini: TIniFile;
  DatabaseType: string;
  PersistenceStr: string;
begin
  Ini := TIniFile.Create(FConfigFile);
  try
    // Read persistence type (FireDAC, UniDAC, or XML)
    PersistenceStr := Ini.ReadString('Database', 'Persistence', 'FireDAC');

    if SameText(PersistenceStr, 'XML') then
      FPersistenceType := ptXML
    {$IFDEF UNIDAC}
    else if SameText(PersistenceStr, 'UniDAC') then
      FPersistenceType := ptUniDAC
    {$ENDIF}
    else
      FPersistenceType := ptFireDAC;

    // Create the appropriate persistence handle
    CreatePersistenceHandle;

    // Configure based on persistence type
    if FPersistenceType = ptXML then
    begin
      ConfigureXML(Ini);
    end
    else
    begin
      // Read database type and configure
      DatabaseType := Ini.ReadString('Database', 'Type', 'MSSQL');

      if SameText(DatabaseType, 'MSSQL') then
        ConfigureMSSQL(Ini)
      else if SameText(DatabaseType, 'PostgreSQL') then
        ConfigurePostgreSQL(Ini)
      else if SameText(DatabaseType, 'Firebird') then
        ConfigureFirebird(Ini)
      else if SameText(DatabaseType, 'SQLite') then
        ConfigureSQLite(Ini);
    end;
  finally
    Ini.Free;
  end;
end;

procedure TDemoDataModule.ConfigureXML(AIni: TIniFile);
var
  FileName: string;
  CacheData: Boolean;
begin
  // Read XML settings
  FileName := AIni.ReadString('XML', 'FileName', ChangeFileExt(ParamStr(0), '.xml'));
  CacheData := AIni.ReadBool('XML', 'CacheData', True);

  // Make path absolute if relative
  if not TPath.IsPathRooted(FileName) then
    FileName := TPath.Combine(ExtractFilePath(ParamStr(0)), FileName);

  FXMLFileName := FileName;
  FPersistenceHandleXML.FileName := FileName;
  FPersistenceHandleXML.CacheData := CacheData;
end;

procedure TDemoDataModule.ConfigureMSSQL(AIni: TIniFile);
var
  Server, Database, User, Password: string;
begin
  Server := AIni.ReadString('MSSQL', 'Server', 'localhost');
  Database := AIni.ReadString('MSSQL', 'Database', 'BoldDemo');
  User := AIni.ReadString('MSSQL', 'User', 'sa');
  Password := AIni.ReadString('MSSQL', 'Password', '');

  case FPersistenceType of
    ptFireDAC:
      begin
        FFDConnection.Params.Clear;
        FFDConnection.Params.Add('DriverID=MSSQL');
        FFDConnection.Params.Add('Server=' + Server);
        FFDConnection.Params.Add('Database=' + Database);
        if AIni.ReadBool('MSSQL', 'OSAuthent', True) then
          FFDConnection.Params.Add('OSAuthent=Yes')
        else
        begin
          FFDConnection.Params.Add('User_Name=' + User);
          FFDConnection.Params.Add('Password=' + Password);
        end;
        FFireDACAdapter.DatabaseEngine := dbeSQLServer;
        ConfigureSQLDatabaseConfig(FFireDACAdapter.SQLDatabaseConfig, 'MSSQL');
      end;
    {$IFDEF UNIDAC}
    ptUniDAC:
      begin
        FUniConnection.ProviderName := 'SQL Server';
        FUniConnection.Server := Server;
        FUniConnection.Database := Database;
        if AIni.ReadBool('MSSQL', 'OSAuthent', True) then
          FUniConnection.SpecificOptions.Values['SQL Server.Authentication'] := 'auWindows'
        else
        begin
          FUniConnection.Username := User;
          FUniConnection.Password := Password;
        end;
        FUniDACAdapter.DatabaseEngine := dbeSQLServer;
        ConfigureSQLDatabaseConfig(FUniDACAdapter.SQLDatabaseConfig, 'MSSQL');
      end;
    {$ENDIF}
  end;
end;

procedure TDemoDataModule.ConfigurePostgreSQL(AIni: TIniFile);
var
  Server, Database, User, Password: string;
  Port: Integer;
begin
  Server := AIni.ReadString('PostgreSQL', 'Server', 'localhost');
  Port := AIni.ReadInteger('PostgreSQL', 'Port', 5432);
  Database := AIni.ReadString('PostgreSQL', 'Database', 'bolddemo');
  User := AIni.ReadString('PostgreSQL', 'User', 'postgres');
  Password := AIni.ReadString('PostgreSQL', 'Password', '');

  case FPersistenceType of
    ptFireDAC:
      begin
        FFDConnection.Params.Clear;
        FFDConnection.Params.Add('DriverID=PG');
        FFDConnection.Params.Add('Server=' + Server);
        FFDConnection.Params.Add('Port=' + IntToStr(Port));
        FFDConnection.Params.Add('Database=' + Database);
        FFDConnection.Params.Add('User_Name=' + User);
        FFDConnection.Params.Add('Password=' + Password);
        FFireDACAdapter.DatabaseEngine := dbePostgres;
        ConfigureSQLDatabaseConfig(FFireDACAdapter.SQLDatabaseConfig, 'PostgreSQL');
      end;
    {$IFDEF UNIDAC}
    ptUniDAC:
      begin
        FUniConnection.ProviderName := 'PostgreSQL';
        FUniConnection.Server := Server;
        FUniConnection.Port := Port;
        FUniConnection.Database := Database;
        FUniConnection.Username := User;
        FUniConnection.Password := Password;
        FUniDACAdapter.DatabaseEngine := dbePostgres;
        ConfigureSQLDatabaseConfig(FUniDACAdapter.SQLDatabaseConfig, 'PostgreSQL');
      end;
    {$ENDIF}
  end;
end;

procedure TDemoDataModule.ConfigureFirebird(AIni: TIniFile);
var
  Database, User, Password: string;
begin
  Database := AIni.ReadString('Firebird', 'Database', 'BoldDemo.fdb');
  User := AIni.ReadString('Firebird', 'User', 'SYSDBA');
  Password := AIni.ReadString('Firebird', 'Password', 'masterkey');

  case FPersistenceType of
    ptFireDAC:
      begin
        FFDConnection.Params.Clear;
        FFDConnection.Params.Add('DriverID=FB');
        FFDConnection.Params.Add('Database=' + Database);
        FFDConnection.Params.Add('User_Name=' + User);
        FFDConnection.Params.Add('Password=' + Password);
        FFireDACAdapter.DatabaseEngine := dbeInterbaseSQLDialect3;
        ConfigureSQLDatabaseConfig(FFireDACAdapter.SQLDatabaseConfig, 'Firebird');
      end;
    {$IFDEF UNIDAC}
    ptUniDAC:
      begin
        FUniConnection.ProviderName := 'InterBase';
        FUniConnection.Database := Database;
        FUniConnection.Username := User;
        FUniConnection.Password := Password;
        FUniDACAdapter.DatabaseEngine := dbeInterbaseSQLDialect3;
        ConfigureSQLDatabaseConfig(FUniDACAdapter.SQLDatabaseConfig, 'Firebird');
      end;
    {$ENDIF}
  end;
end;

procedure TDemoDataModule.ConfigureSQLite(AIni: TIniFile);
var
  Database: string;
begin
  Database := AIni.ReadString('SQLite', 'Database', 'BoldDemo.db');

  case FPersistenceType of
    ptFireDAC:
      begin
        FFDConnection.Params.Clear;
        FFDConnection.Params.Add('DriverID=SQLite');
        FFDConnection.Params.Add('Database=' + Database);
        FFireDACAdapter.DatabaseEngine := dbeGenericANSISQL92;
        ConfigureSQLDatabaseConfig(FFireDACAdapter.SQLDatabaseConfig, 'SQLite');
      end;
    {$IFDEF UNIDAC}
    ptUniDAC:
      begin
        FUniConnection.ProviderName := 'SQLite';
        FUniConnection.Database := Database;
        FUniDACAdapter.DatabaseEngine := dbeGenericANSISQL92;
        ConfigureSQLDatabaseConfig(FUniDACAdapter.SQLDatabaseConfig, 'SQLite');
      end;
    {$ENDIF}
  end;
end;

procedure TDemoDataModule.EnsureDatabaseExists(const ADatabaseName: string);
var
  Ini: TIniFile;
  DatabaseType: string;
  TempFDConn: TFDConnection;
  {$IFDEF UNIDAC}
  TempUniConn: TUniConnection;
  {$ENDIF}
begin
  // Not applicable for XML persistence
  if FPersistenceType = ptXML then
    Exit;

  if not FileExists(FConfigFile) then
    raise Exception.Create('Configuration file not found: ' + FConfigFile);

  Ini := TIniFile.Create(FConfigFile);
  try
    DatabaseType := Ini.ReadString('Database', 'Type', 'MSSQL');

    case FPersistenceType of
      ptFireDAC:
        begin
          TempFDConn := TFDConnection.Create(nil);
          try
            TempFDConn.LoginPrompt := False;

            if SameText(DatabaseType, 'MSSQL') then
            begin
              TempFDConn.Params.Clear;
              TempFDConn.Params.Add('DriverID=MSSQL');
              TempFDConn.Params.Add('Server=' + Ini.ReadString('MSSQL', 'Server', 'localhost'));
              TempFDConn.Params.Add('Database=master');
              if Ini.ReadBool('MSSQL', 'OSAuthent', True) then
                TempFDConn.Params.Add('OSAuthent=Yes')
              else
              begin
                TempFDConn.Params.Add('User_Name=' + Ini.ReadString('MSSQL', 'User', 'sa'));
                TempFDConn.Params.Add('Password=' + Ini.ReadString('MSSQL', 'Password', ''));
              end;
              TempFDConn.Connected := True;
              TempFDConn.ExecSQL('IF NOT EXISTS (SELECT name FROM sys.databases WHERE name = ' +
                QuotedStr(ADatabaseName) + ') CREATE DATABASE [' + ADatabaseName + ']');
              TempFDConn.Connected := False;
              FFDConnection.Params.Values['Database'] := ADatabaseName;
            end
            else if SameText(DatabaseType, 'PostgreSQL') then
            begin
              TempFDConn.Params.Clear;
              TempFDConn.Params.Add('DriverID=PG');
              TempFDConn.Params.Add('Server=' + Ini.ReadString('PostgreSQL', 'Server', 'localhost'));
              TempFDConn.Params.Add('Database=postgres');
              TempFDConn.Params.Add('User_Name=' + Ini.ReadString('PostgreSQL', 'User', 'postgres'));
              TempFDConn.Params.Add('Password=' + Ini.ReadString('PostgreSQL', 'Password', ''));
              TempFDConn.Connected := True;
              TempFDConn.ExecSQL('CREATE DATABASE ' + ADatabaseName, True);
              TempFDConn.Connected := False;
              FFDConnection.Params.Values['Database'] := ADatabaseName;
            end
            else if SameText(DatabaseType, 'Firebird') then
            begin
              if not FileExists(ADatabaseName) then
              begin
                TempFDConn.Params.Clear;
                TempFDConn.Params.Add('DriverID=FB');
                TempFDConn.Params.Add('Database=' + ADatabaseName);
                TempFDConn.Params.Add('User_Name=' + Ini.ReadString('Firebird', 'User', 'SYSDBA'));
                TempFDConn.Params.Add('Password=' + Ini.ReadString('Firebird', 'Password', 'masterkey'));
                TempFDConn.Params.Add('CreateDatabase=Yes');
                TempFDConn.Connected := True;
                TempFDConn.Connected := False;
              end;
              FFDConnection.Params.Values['Database'] := ADatabaseName;
            end
            else if SameText(DatabaseType, 'SQLite') then
            begin
              // SQLite creates the database file automatically when connecting
              FFDConnection.Params.Values['Database'] := ADatabaseName;
            end;
          finally
            TempFDConn.Free;
          end;
        end;

      {$IFDEF UNIDAC}
      ptUniDAC:
        begin
          TempUniConn := TUniConnection.Create(nil);
          try
            TempUniConn.LoginPrompt := False;

            if SameText(DatabaseType, 'MSSQL') then
            begin
              TempUniConn.ProviderName := 'SQL Server';
              TempUniConn.Server := Ini.ReadString('MSSQL', 'Server', 'localhost');
              TempUniConn.Database := 'master';
              if Ini.ReadBool('MSSQL', 'OSAuthent', True) then
                TempUniConn.SpecificOptions.Values['SQL Server.Authentication'] := 'auWindows'
              else
              begin
                TempUniConn.Username := Ini.ReadString('MSSQL', 'User', 'sa');
                TempUniConn.Password := Ini.ReadString('MSSQL', 'Password', '');
              end;
              TempUniConn.Connected := True;
              TempUniConn.ExecSQL('IF NOT EXISTS (SELECT name FROM sys.databases WHERE name = ' +
                QuotedStr(ADatabaseName) + ') CREATE DATABASE [' + ADatabaseName + ']');
              TempUniConn.Connected := False;
              FUniConnection.Database := ADatabaseName;
            end
            else if SameText(DatabaseType, 'PostgreSQL') then
            begin
              TempUniConn.ProviderName := 'PostgreSQL';
              TempUniConn.Server := Ini.ReadString('PostgreSQL', 'Server', 'localhost');
              TempUniConn.Port := Ini.ReadInteger('PostgreSQL', 'Port', 5432);
              TempUniConn.Database := 'postgres';
              TempUniConn.Username := Ini.ReadString('PostgreSQL', 'User', 'postgres');
              TempUniConn.Password := Ini.ReadString('PostgreSQL', 'Password', '');
              TempUniConn.Connected := True;
              try
                TempUniConn.ExecSQL('CREATE DATABASE ' + ADatabaseName);
              except
                // Database may already exist
              end;
              TempUniConn.Connected := False;
              FUniConnection.Database := ADatabaseName;
            end
            else if SameText(DatabaseType, 'Firebird') then
            begin
              // For InterBase/Firebird, the database file is created automatically
              FUniConnection.Database := ADatabaseName;
            end
            else if SameText(DatabaseType, 'SQLite') then
            begin
              // SQLite creates the database file automatically when connecting
              FUniConnection.Database := ADatabaseName;
            end;
          finally
            TempUniConn.Free;
          end;
        end;
      {$ENDIF}
    end;
  finally
    Ini.Free;
  end;
end;

procedure TDemoDataModule.CreateDatabaseSchema;
begin
  if Assigned(FPersistenceHandleDB) then
    FPersistenceHandleDB.CreateDataBaseSchema;
  // XML persistence doesn't need schema creation
end;

function TDemoDataModule.GetConnected: Boolean;
begin
  case FPersistenceType of
    ptFireDAC:
      Result := Assigned(FFDConnection) and FFDConnection.Connected;
    {$IFDEF UNIDAC}
    ptUniDAC:
      Result := Assigned(FUniConnection) and FUniConnection.Connected;
    {$ENDIF}
    ptXML:
      Result := Assigned(FPersistenceHandleXML) and (FXMLFileName <> '');
  else
    Result := False;
  end;
end;

function TDemoDataModule.GetDatabaseName: string;
begin
  case FPersistenceType of
    ptFireDAC:
      if Assigned(FFDConnection) then
        Result := FFDConnection.Params.Values['Database']
      else
        Result := '';
    {$IFDEF UNIDAC}
    ptUniDAC:
      if Assigned(FUniConnection) then
        Result := FUniConnection.Database
      else
        Result := '';
    {$ENDIF}
    ptXML:
      Result := FXMLFileName;
  else
    Result := '';
  end;
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
