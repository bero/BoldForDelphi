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
  System.StrUtils,
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

{$REGION 'Constants'}
const
  // INI Section names
  cSectionDatabase = 'Database';
  cSectionMSSQL = 'MSSQL';
  cSectionPostgreSQL = 'PostgreSQL';
  cSectionFirebird = 'Firebird';
  cSectionSQLite = 'SQLite';
  cSectionXML = 'XML';

  // INI Key names
  cKeyPersistence = 'Persistence';
  cKeyType = 'Type';
  cKeyServer = 'Server';
  cKeyDatabase = 'Database';
  cKeyUser = 'User';
  cKeyPassword = 'Password';
  cKeyOSAuthent = 'OSAuthent';
  cKeyPort = 'Port';
  cKeyVendorLib = 'VendorLib';
  cKeyFileName = 'FileName';
  cKeyCacheData = 'CacheData';

  // Persistence type strings
  cPersistenceFireDAC = 'FireDAC';
  cPersistenceUniDAC = 'UniDAC';
  cPersistenceXML = 'XML';

  // Database type strings (must match INI values)
  cDbTypeMSSQL = 'MSSQL';
  cDbTypePostgreSQL = 'PostgreSQL';
  cDbTypeFirebird = 'Firebird';
  cDbTypeSQLite = 'SQLite';

  // FireDAC Driver IDs
  cDriverMSSQL = 'MSSQL';
  cDriverPG = 'PG';
  cDriverFB = 'FB';
  cDriverSQLite = 'SQLite';

  // FireDAC Parameter names
  cParamDriverID = 'DriverID';
  cParamServer = 'Server';
  cParamDatabase = 'Database';
  cParamPort = 'Port';
  cParamUserName = 'User_Name';
  cParamPassword = 'Password';
  cParamOSAuthent = 'OSAuthent';
  cParamVendorLib = 'VendorLib';
  cParamCreateDatabase = 'CreateDatabase';
  cValueYes = 'Yes';

  // UniDAC Provider names
  cProviderSQLServer = 'SQL Server';
  cProviderPostgreSQL = 'PostgreSQL';
  cProviderInterBase = 'InterBase';
  cProviderSQLite = 'SQLite';
  cUniDACAuthWindows = 'auWindows';
  cUniDACAuthOption = 'SQL Server.Authentication';

  // Admin/System database names
  cAdminDbMaster = 'master';
  cAdminDbPostgres = 'postgres';

  // Default values
  cDefaultServer = 'localhost';
  cDefaultMSSQLDatabase = 'BoldDemo';
  cDefaultMSSQLUser = 'sa';
  cDefaultPostgreSQLDatabase = 'bolddemo';
  cDefaultPostgreSQLUser = 'postgres';
  cDefaultPostgreSQLPort = 5432;
  cDefaultFirebirdDatabase = 'BoldDemo.fdb';
  cDefaultFirebirdUser = 'SYSDBA';
  cDefaultFirebirdPassword = 'masterkey';
  cDefaultSQLiteDatabase = 'BoldDemo.db';

  // SQL Column types - MSSQL
  cMSSQLDate = 'DATE';
  cMSSQLTime = 'TIME';
  cMSSQLDateTime = 'DATETIME';
  cMSSQLBlob = 'VARBINARY(MAX)';
  cMSSQLFloat = 'FLOAT';
  cMSSQLCurrency = 'DECIMAL(18,4)';
  cMSSQLString = 'NVARCHAR(%d)';
  cMSSQLText = 'NVARCHAR(MAX)';
  cMSSQLAnsiString = 'VARCHAR(%d)';
  cMSSQLAnsiText = 'VARCHAR(MAX)';
  cMSSQLInteger = 'INT';
  cMSSQLSmallInt = 'SMALLINT';
  cMSSQLBigInt = 'BIGINT';

  // SQL Column types - PostgreSQL
  cPGDate = 'DATE';
  cPGTime = 'TIME';
  cPGDateTime = 'TIMESTAMP';
  cPGBlob = 'BYTEA';
  cPGFloat = 'DOUBLE PRECISION';
  cPGCurrency = 'DECIMAL(18,4)';
  cPGString = 'VARCHAR(%d)';
  cPGText = 'TEXT';
  cPGInteger = 'INTEGER';
  cPGSmallInt = 'SMALLINT';
  cPGBigInt = 'BIGINT';

  // SQL Column types - Firebird
  cFBDate = 'DATE';
  cFBTime = 'TIME';
  cFBDateTime = 'TIMESTAMP';
  cFBBlob = 'BLOB';
  cFBFloat = 'DOUBLE PRECISION';
  cFBCurrency = 'DECIMAL(18,4)';
  cFBString = 'VARCHAR(%d)';
  cFBText = 'BLOB SUB_TYPE TEXT';
  cFBInteger = 'INTEGER';
  cFBSmallInt = 'SMALLINT';
  cFBBigInt = 'BIGINT';

  // SQL Column types - SQLite
  cSQLiteDate = 'TEXT';
  cSQLiteTime = 'TEXT';
  cSQLiteDateTime = 'TEXT';
  cSQLiteBlob = 'BLOB';
  cSQLiteFloat = 'REAL';
  cSQLiteString = 'TEXT';
  cSQLiteInteger = 'INTEGER';

  // SQL Queries
  cSQLMSSQLCheckDb = 'SELECT 1 FROM sys.databases WHERE name = ';
  cSQLPGCheckDb = 'SELECT 1 FROM pg_database WHERE datname = ';
  cSQLMSSQLDropDb = 'DROP DATABASE [%s]';
  cSQLPGDropDb = 'DROP DATABASE "%s"';

  // Misc
  cSharedFolder = 'Shared';
  cModelFileName = 'DemoModel.bld';
{$ENDREGION}

type
  TPersistenceType = (ptFireDAC, {$IFDEF UNIDAC}ptUniDAC,{$ENDIF} ptXML);
  TDatabaseType = (dtUnknown, dtMSSQL, dtPostgreSQL, dtFirebird, dtSQLite);

function StringToDatabaseType(const AType: string): TDatabaseType;

type
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
    // Configuration
    FConfigFile: string;
    FPersistenceType: TPersistenceType;
    FDatabaseType: TDatabaseType;

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

    // Property getters
    function GetPersistenceTypeStr: string;
    function GetDatabaseTypeStr: string;

    procedure LoadConfiguration;
    procedure CreatePersistenceHandle;
    procedure ConfigureMSSQL(AIni: TIniFile);
    procedure ConfigurePostgreSQL(AIni: TIniFile);
    procedure ConfigureFirebird(AIni: TIniFile);
    procedure ConfigureSQLite(AIni: TIniFile);
    procedure ConfigureXML(AIni: TIniFile);
    procedure ConfigureSQLDatabaseConfig(AConfig: TBoldSQLDatabaseConfig; ADatabaseType: TDatabaseType);
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
    property PersistenceTypeStr: string read GetPersistenceTypeStr;
    property DatabaseType: TDatabaseType read FDatabaseType;
    property DatabaseTypeStr: string read GetDatabaseTypeStr;
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

function StringToDatabaseType(const AType: string): TDatabaseType;
begin
  case IndexText(AType, [cDbTypeMSSQL, cDbTypePostgreSQL, cDbTypeFirebird, cDbTypeSQLite]) of
    0: Result := dtMSSQL;
    1: Result := dtPostgreSQL;
    2: Result := dtFirebird;
    3: Result := dtSQLite;
  else
    Result := dtUnknown;
  end;
end;

function TDemoDataModule.GetPersistenceTypeStr: string;
begin
  case FPersistenceType of
    ptFireDAC: Result := 'FireDAC';
    {$IFDEF UNIDAC}
    ptUniDAC: Result := 'UniDAC';
    {$ENDIF}
    ptXML: Result := 'XML';
  else
    Result := 'Unknown';
  end;
end;

function TDemoDataModule.GetDatabaseTypeStr: string;
begin
  case FDatabaseType of
    dtMSSQL: Result := 'MSSQL';
    dtPostgreSQL: Result := 'PostgreSQL';
    dtFirebird: Result := 'Firebird';
    dtSQLite: Result := 'SQLite';
  else
    Result := 'Unknown';
  end;
end;

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

procedure TDemoDataModule.ConfigureSQLDatabaseConfig(AConfig: TBoldSQLDatabaseConfig; ADatabaseType: TDatabaseType);
begin
  case ADatabaseType of
    dtMSSQL:
      begin
        AConfig.ColumnTypeForDate := cMSSQLDate;
        AConfig.ColumnTypeForTime := cMSSQLTime;
        AConfig.ColumnTypeForDateTime := cMSSQLDateTime;
        AConfig.ColumnTypeForBlob := cMSSQLBlob;
        AConfig.ColumnTypeForFloat := cMSSQLFloat;
        AConfig.ColumnTypeForCurrency := cMSSQLCurrency;
        AConfig.ColumnTypeForString := cMSSQLString;
        AConfig.ColumnTypeForUnicodeString := cMSSQLString;
        AConfig.ColumnTypeForAnsiString := cMSSQLAnsiString;
        AConfig.ColumnTypeForText := cMSSQLText;
        AConfig.ColumnTypeForUnicodeText := cMSSQLText;
        AConfig.ColumnTypeForAnsiText := cMSSQLAnsiText;
        AConfig.ColumnTypeForInteger := cMSSQLInteger;
        AConfig.ColumnTypeForSmallInt := cMSSQLSmallInt;
        AConfig.ColumnTypeForInt64 := cMSSQLBigInt;
      end;
    dtPostgreSQL:
      begin
        AConfig.ColumnTypeForDate := cPGDate;
        AConfig.ColumnTypeForTime := cPGTime;
        AConfig.ColumnTypeForDateTime := cPGDateTime;
        AConfig.ColumnTypeForBlob := cPGBlob;
        AConfig.ColumnTypeForFloat := cPGFloat;
        AConfig.ColumnTypeForCurrency := cPGCurrency;
        AConfig.ColumnTypeForString := cPGString;
        AConfig.ColumnTypeForUnicodeString := cPGString;
        AConfig.ColumnTypeForAnsiString := cPGString;
        AConfig.ColumnTypeForText := cPGText;
        AConfig.ColumnTypeForUnicodeText := cPGText;
        AConfig.ColumnTypeForAnsiText := cPGText;
        AConfig.ColumnTypeForInteger := cPGInteger;
        AConfig.ColumnTypeForSmallInt := cPGSmallInt;
        AConfig.ColumnTypeForInt64 := cPGBigInt;
      end;
    dtFirebird:
      begin
        AConfig.ColumnTypeForDate := cFBDate;
        AConfig.ColumnTypeForTime := cFBTime;
        AConfig.ColumnTypeForDateTime := cFBDateTime;
        AConfig.ColumnTypeForBlob := cFBBlob;
        AConfig.ColumnTypeForFloat := cFBFloat;
        AConfig.ColumnTypeForCurrency := cFBCurrency;
        AConfig.ColumnTypeForString := cFBString;
        AConfig.ColumnTypeForUnicodeString := cFBString;
        AConfig.ColumnTypeForAnsiString := cFBString;
        AConfig.ColumnTypeForText := cFBText;
        AConfig.ColumnTypeForUnicodeText := cFBText;
        AConfig.ColumnTypeForAnsiText := cFBText;
        AConfig.ColumnTypeForInteger := cFBInteger;
        AConfig.ColumnTypeForSmallInt := cFBSmallInt;
        AConfig.ColumnTypeForInt64 := cFBBigInt;
      end;
    dtSQLite:
      begin
        // Column types
        AConfig.ColumnTypeForDate := cSQLiteDate;
        AConfig.ColumnTypeForTime := cSQLiteTime;
        AConfig.ColumnTypeForDateTime := cSQLiteDateTime;
        AConfig.ColumnTypeForBlob := cSQLiteBlob;
        AConfig.ColumnTypeForFloat := cSQLiteFloat;
        AConfig.ColumnTypeForCurrency := cSQLiteFloat;
        AConfig.ColumnTypeForString := cSQLiteString;
        AConfig.ColumnTypeForUnicodeString := cSQLiteString;
        AConfig.ColumnTypeForAnsiString := cSQLiteString;
        AConfig.ColumnTypeForText := cSQLiteString;
        AConfig.ColumnTypeForUnicodeText := cSQLiteString;
        AConfig.ColumnTypeForAnsiText := cSQLiteString;
        AConfig.ColumnTypeForInteger := cSQLiteInteger;
        AConfig.ColumnTypeForSmallInt := cSQLiteInteger;
        AConfig.ColumnTypeForInt64 := cSQLiteInteger;

        // SQLite-specific templates (no schema qualification with periods)
        AConfig.DropIndexTemplate := 'DROP INDEX IF EXISTS <IndexName>';
        AConfig.DropTableTemplate := 'DROP TABLE IF EXISTS <TableName>';
        AConfig.TableExistsTemplate := 'SELECT name FROM sqlite_master WHERE type=''table'' AND name=''<TableName>''';
        AConfig.IndexExistsTemplate := 'SELECT name FROM sqlite_master WHERE type=''index'' AND name=''<IndexName>''';
        AConfig.ColumnExistsTemplate := 'SELECT name FROM pragma_table_info(''<TableName>'') WHERE name=''<ColumnName>''';

        // SQLite supports standard SQL-92 joins and constraints in CREATE TABLE
        AConfig.UseSQL92Joins := True;
        AConfig.SupportsConstraintsInCreateTable := True;

        // SQLite identifier limits
        AConfig.MaxDbIdentifierLength := 128;  // SQLite has no real limit, use reasonable value
      end;
  end;
end;

procedure TDemoDataModule.LoadConfiguration;
var
  Ini: TIniFile;
  PersistenceStr: string;
begin
  Ini := TIniFile.Create(FConfigFile);
  try
    // Read persistence type (FireDAC, UniDAC, or XML)
    PersistenceStr := Ini.ReadString(cSectionDatabase, cKeyPersistence, cPersistenceFireDAC);

    if SameText(PersistenceStr, cPersistenceXML) then
      FPersistenceType := ptXML
    {$IFDEF UNIDAC}
    else if SameText(PersistenceStr, cPersistenceUniDAC) then
      FPersistenceType := ptUniDAC
    {$ENDIF}
    else
      FPersistenceType := ptFireDAC;

    // Create the appropriate persistence handle
    CreatePersistenceHandle;

    // Configure based on persistence type
    if FPersistenceType = ptXML then
    begin
      FDatabaseType := dtUnknown;  // XML doesn't use database type
      ConfigureXML(Ini);
    end
    else
    begin
      // Read database type and configure
      FDatabaseType := StringToDatabaseType(Ini.ReadString(cSectionDatabase, cKeyType, cDbTypeMSSQL));
      case FDatabaseType of
        dtMSSQL: ConfigureMSSQL(Ini);
        dtPostgreSQL: ConfigurePostgreSQL(Ini);
        dtFirebird: ConfigureFirebird(Ini);
        dtSQLite: ConfigureSQLite(Ini);
      end;
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
  FileName := AIni.ReadString(cSectionXML, cKeyFileName, ChangeFileExt(ParamStr(0), '.xml'));
  CacheData := AIni.ReadBool(cSectionXML, cKeyCacheData, True);

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
  Server := AIni.ReadString(cSectionMSSQL, cKeyServer, cDefaultServer);
  Database := AIni.ReadString(cSectionMSSQL, cKeyDatabase, cDefaultMSSQLDatabase);
  User := AIni.ReadString(cSectionMSSQL, cKeyUser, cDefaultMSSQLUser);
  Password := AIni.ReadString(cSectionMSSQL, cKeyPassword, '');

  case FPersistenceType of
    ptFireDAC:
      begin
        FFDConnection.Params.Clear;
        FFDConnection.Params.Add(cParamDriverID + '=' + cDriverMSSQL);
        FFDConnection.Params.Add(cParamServer + '=' + Server);
        FFDConnection.Params.Add(cParamDatabase + '=' + Database);
        if AIni.ReadBool(cSectionMSSQL, cKeyOSAuthent, True) then
          FFDConnection.Params.Add(cParamOSAuthent + '=' + cValueYes)
        else
        begin
          FFDConnection.Params.Add(cParamUserName + '=' + User);
          FFDConnection.Params.Add(cParamPassword + '=' + Password);
        end;
        FFireDACAdapter.DatabaseEngine := dbeSQLServer;
        ConfigureSQLDatabaseConfig(FFireDACAdapter.SQLDatabaseConfig, dtMSSQL);
      end;
    {$IFDEF UNIDAC}
    ptUniDAC:
      begin
        FUniConnection.ProviderName := cProviderSQLServer;
        FUniConnection.Server := Server;
        FUniConnection.Database := Database;
        if AIni.ReadBool(cSectionMSSQL, cKeyOSAuthent, True) then
          FUniConnection.SpecificOptions.Values[cUniDACAuthOption] := cUniDACAuthWindows
        else
        begin
          FUniConnection.Username := User;
          FUniConnection.Password := Password;
        end;
        FUniDACAdapter.DatabaseEngine := dbeSQLServer;
        ConfigureSQLDatabaseConfig(FUniDACAdapter.SQLDatabaseConfig, dtMSSQL);
      end;
    {$ENDIF}
  end;
end;

procedure TDemoDataModule.ConfigurePostgreSQL(AIni: TIniFile);
var
  Server, Database, User, Password, VendorLib: string;
  Port: Integer;
begin
  Server := AIni.ReadString(cSectionPostgreSQL, cKeyServer, cDefaultServer);
  Port := AIni.ReadInteger(cSectionPostgreSQL, cKeyPort, cDefaultPostgreSQLPort);
  Database := AIni.ReadString(cSectionPostgreSQL, cKeyDatabase, cDefaultPostgreSQLDatabase);
  User := AIni.ReadString(cSectionPostgreSQL, cKeyUser, cDefaultPostgreSQLUser);
  Password := AIni.ReadString(cSectionPostgreSQL, cKeyPassword, '');
  VendorLib := AIni.ReadString(cSectionPostgreSQL, cKeyVendorLib, '');

  case FPersistenceType of
    ptFireDAC:
      begin
        FFDConnection.Params.Clear;
        FFDConnection.Params.Add(cParamDriverID + '=' + cDriverPG);
        FFDConnection.Params.Add(cParamServer + '=' + Server);
        FFDConnection.Params.Add(cParamPort + '=' + IntToStr(Port));
        FFDConnection.Params.Add(cParamDatabase + '=' + Database);
        FFDConnection.Params.Add(cParamUserName + '=' + User);
        FFDConnection.Params.Add(cParamPassword + '=' + Password);
        if VendorLib <> '' then
          FFDConnection.Params.Add(cParamVendorLib + '=' + VendorLib);
        FFireDACAdapter.DatabaseEngine := dbePostgres;
        ConfigureSQLDatabaseConfig(FFireDACAdapter.SQLDatabaseConfig, dtPostgreSQL);
      end;
    {$IFDEF UNIDAC}
    ptUniDAC:
      begin
        FUniConnection.ProviderName := cProviderPostgreSQL;
        FUniConnection.Server := Server;
        FUniConnection.Port := Port;
        FUniConnection.Database := Database;
        FUniConnection.Username := User;
        FUniConnection.Password := Password;
        FUniDACAdapter.DatabaseEngine := dbePostgres;
        ConfigureSQLDatabaseConfig(FUniDACAdapter.SQLDatabaseConfig, dtPostgreSQL);
      end;
    {$ENDIF}
  end;
end;

procedure TDemoDataModule.ConfigureFirebird(AIni: TIniFile);
var
  Database, Server, User, Password, VendorLib: string;
begin
  Database := AIni.ReadString(cSectionFirebird, cKeyDatabase, cDefaultFirebirdDatabase);
  Server := AIni.ReadString(cSectionFirebird, cKeyServer, cDefaultServer);
  User := AIni.ReadString(cSectionFirebird, cKeyUser, cDefaultFirebirdUser);
  Password := AIni.ReadString(cSectionFirebird, cKeyPassword, cDefaultFirebirdPassword);
  VendorLib := AIni.ReadString(cSectionFirebird, cKeyVendorLib, '');

  case FPersistenceType of
    ptFireDAC:
      begin
        FFDConnection.Params.Clear;
        FFDConnection.Params.Add(cParamDriverID + '=' + cDriverFB);
        FFDConnection.Params.Add(cParamServer + '=' + Server);
        FFDConnection.Params.Add(cParamDatabase + '=' + Database);
        FFDConnection.Params.Add(cParamUserName + '=' + User);
        FFDConnection.Params.Add(cParamPassword + '=' + Password);
        if VendorLib <> '' then
          FFDConnection.Params.Add(cParamVendorLib + '=' + VendorLib);
        FFireDACAdapter.DatabaseEngine := dbeInterbaseSQLDialect3;
        ConfigureSQLDatabaseConfig(FFireDACAdapter.SQLDatabaseConfig, dtFirebird);
      end;
    {$IFDEF UNIDAC}
    ptUniDAC:
      begin
        FUniConnection.ProviderName := cProviderInterBase;
        FUniConnection.Database := Database;
        FUniConnection.Username := User;
        FUniConnection.Password := Password;
        FUniDACAdapter.DatabaseEngine := dbeInterbaseSQLDialect3;
        ConfigureSQLDatabaseConfig(FUniDACAdapter.SQLDatabaseConfig, dtFirebird);
      end;
    {$ENDIF}
  end;
end;

procedure TDemoDataModule.ConfigureSQLite(AIni: TIniFile);
var
  Database: string;
begin
  Database := AIni.ReadString(cSectionSQLite, cKeyDatabase, cDefaultSQLiteDatabase);

  case FPersistenceType of
    ptFireDAC:
      begin
        FFDConnection.Params.Clear;
        FFDConnection.Params.Add(cParamDriverID + '=' + cDriverSQLite);
        FFDConnection.Params.Add(cParamDatabase + '=' + Database);
        FFireDACAdapter.DatabaseEngine := dbeGenericANSISQL92;
        ConfigureSQLDatabaseConfig(FFireDACAdapter.SQLDatabaseConfig, dtSQLite);
      end;
    {$IFDEF UNIDAC}
    ptUniDAC:
      begin
        FUniConnection.ProviderName := cProviderSQLite;
        FUniConnection.Database := Database;
        FUniDACAdapter.DatabaseEngine := dbeGenericANSISQL92;
        ConfigureSQLDatabaseConfig(FUniDACAdapter.SQLDatabaseConfig, dtSQLite);
      end;
    {$ENDIF}
  end;
end;

function TDemoDataModule.DatabaseExists: Boolean;
var
  Ini: TIniFile;
  DbType: TDatabaseType;
  DatabaseName: string;
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
    DbType := StringToDatabaseType(Ini.ReadString(cSectionDatabase, cKeyType, cDbTypeMSSQL));

    case FPersistenceType of
      ptFireDAC:
        begin
          case DbType of
            dtMSSQL:
              begin
                DatabaseName := Ini.ReadString(cSectionMSSQL, cKeyDatabase, cDefaultMSSQLDatabase);
                TempFDConn := TFDConnection.Create(nil);
                Query := TFDQuery.Create(nil);
                try
                  try
                    TempFDConn.LoginPrompt := False;
                    TempFDConn.Params.Clear;
                    TempFDConn.Params.Add(cParamDriverID + '=' + cDriverMSSQL);
                    TempFDConn.Params.Add(cParamServer + '=' + Ini.ReadString(cSectionMSSQL, cKeyServer, cDefaultServer));
                    TempFDConn.Params.Add(cParamDatabase + '=' + cAdminDbMaster);
                    if Ini.ReadBool(cSectionMSSQL, cKeyOSAuthent, True) then
                      TempFDConn.Params.Add(cParamOSAuthent + '=' + cValueYes)
                    else
                    begin
                      TempFDConn.Params.Add(cParamUserName + '=' + Ini.ReadString(cSectionMSSQL, cKeyUser, cDefaultMSSQLUser));
                      TempFDConn.Params.Add(cParamPassword + '=' + Ini.ReadString(cSectionMSSQL, cKeyPassword, ''));
                    end;
                    TempFDConn.Connected := True;
                    Query.Connection := TempFDConn;
                    Query.SQL.Text := cSQLMSSQLCheckDb + QuotedStr(DatabaseName);
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
              end;
            dtPostgreSQL:
              begin
                DatabaseName := Ini.ReadString(cSectionPostgreSQL, cKeyDatabase, cDefaultPostgreSQLDatabase);
                TempFDConn := TFDConnection.Create(nil);
                Query := TFDQuery.Create(nil);
                try
                  try
                    TempFDConn.LoginPrompt := False;
                    TempFDConn.Params.Clear;
                    TempFDConn.Params.Add(cParamDriverID + '=' + cDriverPG);
                    TempFDConn.Params.Add(cParamServer + '=' + Ini.ReadString(cSectionPostgreSQL, cKeyServer, cDefaultServer));
                    TempFDConn.Params.Add(cParamPort + '=' + Ini.ReadString(cSectionPostgreSQL, cKeyPort, IntToStr(cDefaultPostgreSQLPort)));
                    TempFDConn.Params.Add(cParamDatabase + '=' + cAdminDbPostgres);
                    TempFDConn.Params.Add(cParamUserName + '=' + Ini.ReadString(cSectionPostgreSQL, cKeyUser, cDefaultPostgreSQLUser));
                    TempFDConn.Params.Add(cParamPassword + '=' + Ini.ReadString(cSectionPostgreSQL, cKeyPassword, ''));
                    TempFDConn.Connected := True;
                    Query.Connection := TempFDConn;
                    Query.SQL.Text := cSQLPGCheckDb + QuotedStr(DatabaseName);
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
              end;
            dtFirebird:
              begin
                DatabaseName := Ini.ReadString(cSectionFirebird, cKeyDatabase, cDefaultFirebirdDatabase);
                Result := FileExists(DatabaseName);
              end;
            dtSQLite:
              begin
                DatabaseName := Ini.ReadString(cSectionSQLite, cKeyDatabase, cDefaultSQLiteDatabase);
                // For SQLite, check if file exists AND has schema (BOLD_TYPE table)
                if not FileExists(DatabaseName) then
                  Result := False
                else
                begin
                  TempFDConn := TFDConnection.Create(nil);
                  Query := TFDQuery.Create(nil);
                  try
                    try
                      TempFDConn.LoginPrompt := False;
                      TempFDConn.Params.Clear;
                      TempFDConn.Params.Add(cParamDriverID + '=' + cDriverSQLite);
                      TempFDConn.Params.Add(cParamDatabase + '=' + DatabaseName);
                      TempFDConn.Connected := True;
                      Query.Connection := TempFDConn;
                      Query.SQL.Text := 'SELECT name FROM sqlite_master WHERE type=''table'' AND name=''BOLD_TYPE''';
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
                end;
              end;
          end;
        end;

      {$IFDEF UNIDAC}
      ptUniDAC:
        begin
          case DbType of
            dtMSSQL:
              begin
                DatabaseName := Ini.ReadString(cSectionMSSQL, cKeyDatabase, cDefaultMSSQLDatabase);
                TempUniConn := TUniConnection.Create(nil);
                UniQuery := TUniQuery.Create(nil);
                try
                  try
                    TempUniConn.LoginPrompt := False;
                    TempUniConn.ProviderName := cProviderSQLServer;
                    TempUniConn.Server := Ini.ReadString(cSectionMSSQL, cKeyServer, cDefaultServer);
                    TempUniConn.Database := cAdminDbMaster;
                    if Ini.ReadBool(cSectionMSSQL, cKeyOSAuthent, True) then
                      TempUniConn.SpecificOptions.Values[cUniDACAuthOption] := cUniDACAuthWindows
                    else
                    begin
                      TempUniConn.Username := Ini.ReadString(cSectionMSSQL, cKeyUser, cDefaultMSSQLUser);
                      TempUniConn.Password := Ini.ReadString(cSectionMSSQL, cKeyPassword, '');
                    end;
                    TempUniConn.Connected := True;
                    UniQuery.Connection := TempUniConn;
                    UniQuery.SQL.Text := cSQLMSSQLCheckDb + QuotedStr(DatabaseName);
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
              end;
            dtPostgreSQL:
              begin
                DatabaseName := Ini.ReadString(cSectionPostgreSQL, cKeyDatabase, cDefaultPostgreSQLDatabase);
                TempUniConn := TUniConnection.Create(nil);
                UniQuery := TUniQuery.Create(nil);
                try
                  try
                    TempUniConn.LoginPrompt := False;
                    TempUniConn.ProviderName := cProviderPostgreSQL;
                    TempUniConn.Server := Ini.ReadString(cSectionPostgreSQL, cKeyServer, cDefaultServer);
                    TempUniConn.Port := Ini.ReadInteger(cSectionPostgreSQL, cKeyPort, cDefaultPostgreSQLPort);
                    TempUniConn.Database := cAdminDbPostgres;
                    TempUniConn.Username := Ini.ReadString(cSectionPostgreSQL, cKeyUser, cDefaultPostgreSQLUser);
                    TempUniConn.Password := Ini.ReadString(cSectionPostgreSQL, cKeyPassword, '');
                    TempUniConn.Connected := True;
                    UniQuery.Connection := TempUniConn;
                    UniQuery.SQL.Text := cSQLPGCheckDb + QuotedStr(DatabaseName);
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
              end;
            dtFirebird:
              begin
                DatabaseName := Ini.ReadString(cSectionFirebird, cKeyDatabase, cDefaultFirebirdDatabase);
                Result := FileExists(DatabaseName);
              end;
            dtSQLite:
              begin
                DatabaseName := Ini.ReadString(cSectionSQLite, cKeyDatabase, cDefaultSQLiteDatabase);
                // For SQLite, check if file exists AND has schema (BOLD_TYPE table)
                if not FileExists(DatabaseName) then
                  Result := False
                else
                begin
                  TempUniConn := TUniConnection.Create(nil);
                  UniQuery := TUniQuery.Create(nil);
                  try
                    try
                      TempUniConn.LoginPrompt := False;
                      TempUniConn.ProviderName := cProviderSQLite;
                      TempUniConn.Database := DatabaseName;
                      TempUniConn.Connected := True;
                      UniQuery.Connection := TempUniConn;
                      UniQuery.SQL.Text := 'SELECT name FROM sqlite_master WHERE type=''table'' AND name=''BOLD_TYPE''';
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
                end;
              end;
          end;
        end;
      {$ENDIF}
    end;
  finally
    Ini.Free;
  end;
end;

procedure TDemoDataModule.CreateDatabaseIfNotExists;
var
  Ini: TIniFile;
  DbType: TDatabaseType;
begin
  // Not applicable for XML persistence
  if FPersistenceType = ptXML then
    Exit;

  Ini := TIniFile.Create(FConfigFile);
  try
    DbType := StringToDatabaseType(Ini.ReadString(cSectionDatabase, cKeyType, cDbTypeMSSQL));

    case DbType of
      dtFirebird:
        begin
          // For Firebird, use a separate temp connection to create the database
          // This avoids FireDAC caching the CreateDatabase flag on the main connection
          case FPersistenceType of
            ptFireDAC:
              begin
                var TempConn := TFDConnection.Create(nil);
                try
                  TempConn.LoginPrompt := False;
                  TempConn.Params.Assign(FFDConnection.Params);
                  TempConn.Params.Values[cParamServer] := '';  // Use embedded mode for creation
                  TempConn.Params.Values[cParamCreateDatabase] := cValueYes;
                  TempConn.Connected := True;
                  TempConn.Connected := False;
                finally
                  TempConn.Free;
                end;
              end;
            {$IFDEF UNIDAC}
            ptUniDAC:
              ; // UniDAC handles file creation automatically
            {$ENDIF}
          end;
        end;
      dtSQLite:
        ; // SQLite creates the file automatically on first connect
      dtMSSQL, dtPostgreSQL:
        begin
          // MSSQL/PostgreSQL - use Bold's built-in CreateDatabase method
          case FPersistenceType of
            ptFireDAC:
              FFireDACAdapter.CreateDatabase(False);
            {$IFDEF UNIDAC}
            ptUniDAC:
              FUniDACAdapter.CreateDatabase(False);
            {$ENDIF}
          end;
        end;
    end;
  finally
    Ini.Free;
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
  DbType: TDatabaseType;
  DatabaseName: string;
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
    DbType := StringToDatabaseType(Ini.ReadString(cSectionDatabase, cKeyType, cDbTypeMSSQL));

    case FPersistenceType of
      ptFireDAC:
        begin
          TempFDConn := TFDConnection.Create(nil);
          try
            TempFDConn.LoginPrompt := False;

            case DbType of
              dtMSSQL:
                begin
                  DatabaseName := Ini.ReadString(cSectionMSSQL, cKeyDatabase, cDefaultMSSQLDatabase);
                  TempFDConn.Params.Clear;
                  TempFDConn.Params.Add(cParamDriverID + '=' + cDriverMSSQL);
                  TempFDConn.Params.Add(cParamServer + '=' + Ini.ReadString(cSectionMSSQL, cKeyServer, cDefaultServer));
                  TempFDConn.Params.Add(cParamDatabase + '=' + cAdminDbMaster);
                  if Ini.ReadBool(cSectionMSSQL, cKeyOSAuthent, True) then
                    TempFDConn.Params.Add(cParamOSAuthent + '=' + cValueYes)
                  else
                  begin
                    TempFDConn.Params.Add(cParamUserName + '=' + Ini.ReadString(cSectionMSSQL, cKeyUser, cDefaultMSSQLUser));
                    TempFDConn.Params.Add(cParamPassword + '=' + Ini.ReadString(cSectionMSSQL, cKeyPassword, ''));
                  end;
                  TempFDConn.Connected := True;
                  TempFDConn.ExecSQL(Format(cSQLMSSQLDropDb, [DatabaseName]));
                  TempFDConn.Connected := False;
                end;
              dtPostgreSQL:
                begin
                  DatabaseName := Ini.ReadString(cSectionPostgreSQL, cKeyDatabase, cDefaultPostgreSQLDatabase);
                  TempFDConn.Params.Clear;
                  TempFDConn.Params.Add(cParamDriverID + '=' + cDriverPG);
                  TempFDConn.Params.Add(cParamServer + '=' + Ini.ReadString(cSectionPostgreSQL, cKeyServer, cDefaultServer));
                  TempFDConn.Params.Add(cParamPort + '=' + Ini.ReadString(cSectionPostgreSQL, cKeyPort, IntToStr(cDefaultPostgreSQLPort)));
                  TempFDConn.Params.Add(cParamDatabase + '=' + cAdminDbPostgres);
                  TempFDConn.Params.Add(cParamUserName + '=' + Ini.ReadString(cSectionPostgreSQL, cKeyUser, cDefaultPostgreSQLUser));
                  TempFDConn.Params.Add(cParamPassword + '=' + Ini.ReadString(cSectionPostgreSQL, cKeyPassword, ''));
                  TempFDConn.Connected := True;
                  TempFDConn.ExecSQL(Format(cSQLPGDropDb, [DatabaseName]));
                  TempFDConn.Connected := False;
                end;
              dtFirebird:
                begin
                  DatabaseName := Ini.ReadString(cSectionFirebird, cKeyDatabase, cDefaultFirebirdDatabase);
                  if FileExists(DatabaseName) then
                    DeleteFile(DatabaseName);
                end;
              dtSQLite:
                begin
                  DatabaseName := Ini.ReadString(cSectionSQLite, cKeyDatabase, cDefaultSQLiteDatabase);
                  if FileExists(DatabaseName) then
                    DeleteFile(DatabaseName);
                end;
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

            case DbType of
              dtMSSQL:
                begin
                  DatabaseName := Ini.ReadString(cSectionMSSQL, cKeyDatabase, cDefaultMSSQLDatabase);
                  TempUniConn.ProviderName := cProviderSQLServer;
                  TempUniConn.Server := Ini.ReadString(cSectionMSSQL, cKeyServer, cDefaultServer);
                  TempUniConn.Database := cAdminDbMaster;
                  if Ini.ReadBool(cSectionMSSQL, cKeyOSAuthent, True) then
                    TempUniConn.SpecificOptions.Values[cUniDACAuthOption] := cUniDACAuthWindows
                  else
                  begin
                    TempUniConn.Username := Ini.ReadString(cSectionMSSQL, cKeyUser, cDefaultMSSQLUser);
                    TempUniConn.Password := Ini.ReadString(cSectionMSSQL, cKeyPassword, '');
                  end;
                  TempUniConn.Connected := True;
                  TempUniConn.ExecSQL(Format(cSQLMSSQLDropDb, [DatabaseName]));
                  TempUniConn.Connected := False;
                end;
              dtPostgreSQL:
                begin
                  DatabaseName := Ini.ReadString(cSectionPostgreSQL, cKeyDatabase, cDefaultPostgreSQLDatabase);
                  TempUniConn.ProviderName := cProviderPostgreSQL;
                  TempUniConn.Server := Ini.ReadString(cSectionPostgreSQL, cKeyServer, cDefaultServer);
                  TempUniConn.Port := Ini.ReadInteger(cSectionPostgreSQL, cKeyPort, cDefaultPostgreSQLPort);
                  TempUniConn.Database := cAdminDbPostgres;
                  TempUniConn.Username := Ini.ReadString(cSectionPostgreSQL, cKeyUser, cDefaultPostgreSQLUser);
                  TempUniConn.Password := Ini.ReadString(cSectionPostgreSQL, cKeyPassword, '');
                  TempUniConn.Connected := True;
                  TempUniConn.ExecSQL(Format(cSQLPGDropDb, [DatabaseName]));
                  TempUniConn.Connected := False;
                end;
              dtFirebird:
                begin
                  DatabaseName := Ini.ReadString(cSectionFirebird, cKeyDatabase, cDefaultFirebirdDatabase);
                  if FileExists(DatabaseName) then
                    DeleteFile(DatabaseName);
                end;
              dtSQLite:
                begin
                  DatabaseName := Ini.ReadString(cSectionSQLite, cKeyDatabase, cDefaultSQLiteDatabase);
                  if FileExists(DatabaseName) then
                    DeleteFile(DatabaseName);
                end;
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
        Result := FFDConnection.Params.Values[cParamDatabase]
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
        // For file-based DBs (Firebird/SQLite), CreateDatabaseIfNotExists added CreateDatabase=Yes
        // The connection will create the file on first connect
        // For server DBs (MSSQL/PostgreSQL), the database was already created

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
    Result := ExePath + cSharedFolder + PathDelim;
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
    Result := Result + cModelFileName;
end;

end.
