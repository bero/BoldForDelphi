{$INCLUDE bold.inc}
unit DemoDataModule;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IniFiles,
  System.IOUtils,
  System.UITypes,
  System.Actions,
  Data.DB,
  Vcl.ActnList,
  Vcl.Dialogs,

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
  FireDAC.DApt,
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
  BoldActions,
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
  BoldSQLDatabaseConfig, BoldHandleAction;

type
  TPersistenceType = (ptFireDAC, {$IFDEF UNIDAC}ptUniDAC,{$ENDIF} ptXML);

  TDemoDataModule = class(TDataModule)
    BoldModel1: TBoldModel;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    ActionList1: TActionList;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure BoldActivateSystemAction1Execute(Sender: TObject);
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
    procedure CreateDatabaseIfNotExists;
    procedure DropDatabase;
    function DatabaseExists: Boolean;
    procedure OpenSystem;
    procedure CloseSystem;
    function GetConnected: Boolean;
    function GetDatabaseName: string;
    class function GetSharedPath: string;
    class function GetModelFilePath: string;
    property ConfigFile: string read FConfigFile write FConfigFile;
    property PersistenceType: TPersistenceType read FPersistenceType;
    property PersistenceHandleDB: TBoldPersistenceHandleDB read FPersistenceHandleDB;
    property Connected: Boolean read GetConnected;
    property DatabaseName: string read GetDatabaseName;
    property SharedPath: string read GetSharedPath;
    property ModelFilePath: string read GetModelFilePath;
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
  Server, Database, User, Password, VendorHome: string;
  Port: Integer;
begin
  Server := AIni.ReadString('PostgreSQL', 'Server', 'localhost');
  Port := AIni.ReadInteger('PostgreSQL', 'Port', 5432);
  Database := AIni.ReadString('PostgreSQL', 'Database', 'bolddemo');
  User := AIni.ReadString('PostgreSQL', 'User', 'postgres');
  Password := AIni.ReadString('PostgreSQL', 'Password', '');
  VendorHome := AIni.ReadString('PostgreSQL', 'VendorHome', '');

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
        if VendorHome <> '' then
          FFDConnection.Params.Add('VendorHome=' + VendorHome);
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

function TDemoDataModule.DatabaseExists: Boolean;
var
  Ini: TIniFile;
  DatabaseType, DatabaseName: string;
  TempFDConn: TFDConnection;
  Query: TFDQuery;
  {$IFDEF UNIDAC}
  TempUniConn: TUniConnection;
  UniQuery: TUniQuery;
  {$ENDIF}
begin
  Result := False;

  // XML persistence always "exists"
  if FPersistenceType = ptXML then
    Exit(True);

  if not FileExists(FConfigFile) then
    Exit(False);

  Ini := TIniFile.Create(FConfigFile);
  try
    DatabaseType := Ini.ReadString('Database', 'Type', 'MSSQL');

    case FPersistenceType of
      ptFireDAC:
        begin
          if SameText(DatabaseType, 'MSSQL') then
          begin
            DatabaseName := Ini.ReadString('MSSQL', 'Database', 'BoldDemo');
            TempFDConn := TFDConnection.Create(nil);
            Query := TFDQuery.Create(nil);
            try
              try
                TempFDConn.LoginPrompt := False;
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
                Query.Connection := TempFDConn;
                Query.SQL.Text := 'SELECT 1 FROM sys.databases WHERE name = ' + QuotedStr(DatabaseName);
                Query.Open;
                Result := not Query.IsEmpty;
                Query.Close;
                TempFDConn.Connected := False;
              except
                Result := False;
              end;
            finally
              Query.Free;
              TempFDConn.Free;
            end;
          end
          else if SameText(DatabaseType, 'PostgreSQL') then
          begin
            DatabaseName := Ini.ReadString('PostgreSQL', 'Database', 'bolddemo');
            TempFDConn := TFDConnection.Create(nil);
            Query := TFDQuery.Create(nil);
            try
              try
                TempFDConn.LoginPrompt := False;
                TempFDConn.Params.Clear;
                TempFDConn.Params.Add('DriverID=PG');
                TempFDConn.Params.Add('Server=' + Ini.ReadString('PostgreSQL', 'Server', 'localhost'));
                TempFDConn.Params.Add('Port=' + Ini.ReadString('PostgreSQL', 'Port', '5432'));
                TempFDConn.Params.Add('Database=postgres');
                TempFDConn.Params.Add('User_Name=' + Ini.ReadString('PostgreSQL', 'User', 'postgres'));
                TempFDConn.Params.Add('Password=' + Ini.ReadString('PostgreSQL', 'Password', ''));
                TempFDConn.Connected := True;
                Query.Connection := TempFDConn;
                Query.SQL.Text := 'SELECT 1 FROM pg_database WHERE datname = ' + QuotedStr(DatabaseName);
                Query.Open;
                Result := not Query.IsEmpty;
                Query.Close;
                TempFDConn.Connected := False;
              except
                Result := False;
              end;
            finally
              Query.Free;
              TempFDConn.Free;
            end;
          end
          else if SameText(DatabaseType, 'Firebird') then
          begin
            DatabaseName := Ini.ReadString('Firebird', 'Database', 'BoldDemo.fdb');
            Result := FileExists(DatabaseName);
          end
          else if SameText(DatabaseType, 'SQLite') then
          begin
            DatabaseName := Ini.ReadString('SQLite', 'Database', 'BoldDemo.db');
            Result := FileExists(DatabaseName);
          end;
        end;

      {$IFDEF UNIDAC}
      ptUniDAC:
        begin
          if SameText(DatabaseType, 'MSSQL') then
          begin
            DatabaseName := Ini.ReadString('MSSQL', 'Database', 'BoldDemo');
            TempUniConn := TUniConnection.Create(nil);
            UniQuery := TUniQuery.Create(nil);
            try
              try
                TempUniConn.LoginPrompt := False;
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
                UniQuery.Connection := TempUniConn;
                UniQuery.SQL.Text := 'SELECT 1 FROM sys.databases WHERE name = ' + QuotedStr(DatabaseName);
                UniQuery.Open;
                Result := not UniQuery.IsEmpty;
                UniQuery.Close;
                TempUniConn.Connected := False;
              except
                Result := False;
              end;
            finally
              UniQuery.Free;
              TempUniConn.Free;
            end;
          end
          else if SameText(DatabaseType, 'PostgreSQL') then
          begin
            DatabaseName := Ini.ReadString('PostgreSQL', 'Database', 'bolddemo');
            TempUniConn := TUniConnection.Create(nil);
            UniQuery := TUniQuery.Create(nil);
            try
              try
                TempUniConn.LoginPrompt := False;
                TempUniConn.ProviderName := 'PostgreSQL';
                TempUniConn.Server := Ini.ReadString('PostgreSQL', 'Server', 'localhost');
                TempUniConn.Port := Ini.ReadInteger('PostgreSQL', 'Port', 5432);
                TempUniConn.Database := 'postgres';
                TempUniConn.Username := Ini.ReadString('PostgreSQL', 'User', 'postgres');
                TempUniConn.Password := Ini.ReadString('PostgreSQL', 'Password', '');
                TempUniConn.Connected := True;
                UniQuery.Connection := TempUniConn;
                UniQuery.SQL.Text := 'SELECT 1 FROM pg_database WHERE datname = ' + QuotedStr(DatabaseName);
                UniQuery.Open;
                Result := not UniQuery.IsEmpty;
                UniQuery.Close;
                TempUniConn.Connected := False;
              except
                Result := False;
              end;
            finally
              UniQuery.Free;
              TempUniConn.Free;
            end;
          end
          else if SameText(DatabaseType, 'Firebird') then
          begin
            DatabaseName := Ini.ReadString('Firebird', 'Database', 'BoldDemo.fdb');
            Result := FileExists(DatabaseName);
          end
          else if SameText(DatabaseType, 'SQLite') then
          begin
            DatabaseName := Ini.ReadString('SQLite', 'Database', 'BoldDemo.db');
            Result := FileExists(DatabaseName);
          end;
        end;
      {$ENDIF}
    end;
  finally
    Ini.Free;
  end;
end;

procedure TDemoDataModule.CreateDatabaseIfNotExists;
begin
  // Not applicable for XML persistence
  if FPersistenceType = ptXML then
    Exit;

  // Use Bold's built-in CreateDatabase method
  // DropExisting=False means don't drop if it already exists
  case FPersistenceType of
    ptFireDAC:
      FFireDACAdapter.CreateDatabase(False);
    {$IFDEF UNIDAC}
    ptUniDAC:
      FUniDACAdapter.CreateDatabase(False);
    {$ENDIF}
  end;
end;

procedure TDemoDataModule.CreateDatabaseSchema;
begin
  if Assigned(FPersistenceHandleDB) then
    FPersistenceHandleDB.CreateDataBaseSchema(True);  // IgnoreUnknownTables=True to skip system tables
  // XML persistence doesn't need schema creation
end;

procedure TDemoDataModule.DropDatabase;
var
  Ini: TIniFile;
  DatabaseType, DatabaseName: string;
  TempFDConn: TFDConnection;
  {$IFDEF UNIDAC}
  TempUniConn: TUniConnection;
  {$ENDIF}
begin
  // Not applicable for XML persistence
  if FPersistenceType = ptXML then
    Exit;

  // Check if database exists first
  if not DatabaseExists then
    Exit;

  // Disconnect existing connection
  if Assigned(FFDConnection) and FFDConnection.Connected then
    FFDConnection.Connected := False;
  {$IFDEF UNIDAC}
  if Assigned(FUniConnection) and FUniConnection.Connected then
    FUniConnection.Connected := False;
  {$ENDIF}

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
              DatabaseName := Ini.ReadString('MSSQL', 'Database', 'BoldDemo');
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
              TempFDConn.ExecSQL('DROP DATABASE [' + DatabaseName + ']');
              TempFDConn.Connected := False;
            end
            else if SameText(DatabaseType, 'PostgreSQL') then
            begin
              DatabaseName := Ini.ReadString('PostgreSQL', 'Database', 'bolddemo');
              TempFDConn.Params.Clear;
              TempFDConn.Params.Add('DriverID=PG');
              TempFDConn.Params.Add('Server=' + Ini.ReadString('PostgreSQL', 'Server', 'localhost'));
              TempFDConn.Params.Add('Port=' + Ini.ReadString('PostgreSQL', 'Port', '5432'));
              TempFDConn.Params.Add('Database=postgres');
              TempFDConn.Params.Add('User_Name=' + Ini.ReadString('PostgreSQL', 'User', 'postgres'));
              TempFDConn.Params.Add('Password=' + Ini.ReadString('PostgreSQL', 'Password', ''));
              TempFDConn.Connected := True;
              TempFDConn.ExecSQL('DROP DATABASE "' + DatabaseName + '"');
              TempFDConn.Connected := False;
            end
            else if SameText(DatabaseType, 'Firebird') then
            begin
              DatabaseName := Ini.ReadString('Firebird', 'Database', 'BoldDemo.fdb');
              if FileExists(DatabaseName) then
                DeleteFile(DatabaseName);
            end
            else if SameText(DatabaseType, 'SQLite') then
            begin
              DatabaseName := Ini.ReadString('SQLite', 'Database', 'BoldDemo.db');
              if FileExists(DatabaseName) then
                DeleteFile(DatabaseName);
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
              DatabaseName := Ini.ReadString('MSSQL', 'Database', 'BoldDemo');
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
              TempUniConn.ExecSQL('DROP DATABASE [' + DatabaseName + ']');
              TempUniConn.Connected := False;
            end
            else if SameText(DatabaseType, 'PostgreSQL') then
            begin
              DatabaseName := Ini.ReadString('PostgreSQL', 'Database', 'bolddemo');
              TempUniConn.ProviderName := 'PostgreSQL';
              TempUniConn.Server := Ini.ReadString('PostgreSQL', 'Server', 'localhost');
              TempUniConn.Port := Ini.ReadInteger('PostgreSQL', 'Port', 5432);
              TempUniConn.Database := 'postgres';
              TempUniConn.Username := Ini.ReadString('PostgreSQL', 'User', 'postgres');
              TempUniConn.Password := Ini.ReadString('PostgreSQL', 'Password', '');
              TempUniConn.Connected := True;
              TempUniConn.ExecSQL('DROP DATABASE "' + DatabaseName + '"');
              TempUniConn.Connected := False;
            end
            else if SameText(DatabaseType, 'Firebird') then
            begin
              DatabaseName := Ini.ReadString('Firebird', 'Database', 'BoldDemo.fdb');
              if FileExists(DatabaseName) then
                DeleteFile(DatabaseName);
            end
            else if SameText(DatabaseType, 'SQLite') then
            begin
              DatabaseName := Ini.ReadString('SQLite', 'Database', 'BoldDemo.db');
              if FileExists(DatabaseName) then
                DeleteFile(DatabaseName);
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
  // Check if database exists (skip for XML persistence)
  if FPersistenceType <> ptXML then
  begin
    if not DatabaseExists then
    begin
      if MessageDlg('Database "' + DatabaseName + '" does not exist.' + sLineBreak +
                    'Do you want to create it now?',
                    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        CreateDatabaseIfNotExists;
        // Connect to the newly created database before creating the schema
        case FPersistenceType of
          ptFireDAC:
            begin
              FFDConnection.Connected := True;
              if not FFDConnection.Connected then
                raise Exception.Create('Failed to connect to database after creation');
            end;
          {$IFDEF UNIDAC}
          ptUniDAC:
            begin
              FUniConnection.Connected := True;
              if not FUniConnection.Connected then
                raise Exception.Create('Failed to connect to database after creation');
            end;
          {$ENDIF}
        end;
        // Create the Bold schema (tables like bold_type, bold_id, etc.)
        CreateDatabaseSchema;
      end
      else
        Exit;  // User declined, don't open system
    end;
  end;

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

procedure TDemoDataModule.BoldActivateSystemAction1Execute(Sender: TObject);
begin
  if BoldSystemHandle1.Active then
    CloseSystem
  else
    OpenSystem;
end;

class function TDemoDataModule.GetSharedPath: string;
var
  ExePath: string;
  I: Integer;
begin
  // Navigate up from exe location until we find the 'examples' folder
  // Then go to 'Shared' subfolder
  // This works regardless of which example project is running
  ExePath := ExtractFilePath(ParamStr(0));

  // Go up directories until we find one containing 'Shared'
  for I := 1 to 10 do  // Max 10 levels to prevent infinite loop
  begin
    // Remove trailing delimiter and go up one level
    ExePath := IncludeTrailingPathDelimiter(
      ExtractFilePath(ExcludeTrailingPathDelimiter(ExePath)));

    // Check if Shared exists at this level
    Result := ExePath + 'Shared' + PathDelim;
    if DirectoryExists(Result) then
      Exit;
  end;

  // Fallback: return empty if not found
  Result := '';
end;

class function TDemoDataModule.GetModelFilePath: string;
begin
  Result := GetSharedPath;
  if Result <> '' then
    Result := Result + 'DemoModel.bld';
end;

end.
