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
  Vcl.Forms,

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

  // FireDAC drivers - include all supported databases
  // This uses must be here to register database specific drivers
  FireDAC.Phys.MSSQL,
  FireDAC.Phys.PG,
  FireDAC.Phys.FB,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.MySQL,
  FireDAC.Phys.Oracle,

  {$IFDEF UNIDAC}
  // UniDAC (optional - define UNIDAC in project options if available)
  Uni,
  BoldDatabaseAdapterUniDAC,
  {$ENDIF}

  // Bold
  BoldAbstractDatabaseAdapter,
  BoldAbstractModel,
  BoldAbstractPersistenceHandleDB,
  BoldActions,
  BoldDatabaseAdapterFireDAC,
  BoldDefs,
  BoldHandle,
  BoldHandleAction,
  BoldHandles,
  BoldModel,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldPersistenceHandleFileXML,
  BoldSQLDatabaseConfig,
  BoldSubscription,
  BoldSystem,
  BoldSystemHandle;

{$REGION 'Constants'}
const
  // INI Section names
  cSectionDatabase = 'Database';
  cSectionMSSQL = 'MSSQL';
  cSectionPostgreSQL = 'PostgreSQL';
  cSectionFirebird = 'Firebird';
  cSectionSQLite = 'SQLite';
  cSectionMariaDB = 'MariaDB';
  cSectionOracle = 'Oracle';
  cSectionXML = 'XML';

  // INI Key names
  cKeyPersistence = 'Persistence';
  cKeyType = 'Type';
  cKeyServer = 'Server';
  cKeyDatabase = 'Database';
  cKeyUser = 'User';
  cKeyPassword = 'Password';
  cKeyAdminUser = 'AdminUser';
  cKeyAdminPassword = 'AdminPassword';
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
  cDbTypeMariaDB = 'MariaDB';
  cDbTypeOracle = 'Oracle';

  // FireDAC Driver IDs
  cDriverMSSQL = 'MSSQL';
  cDriverPG = 'PG';
  cDriverFB = 'FB';
  cDriverMySQL = 'MySQL';
  cDriverSQLite = 'SQLite';
  cDriverOracle = 'Ora';

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
  cDefaultMariaDBDatabase = 'bolddemo';
  cDefaultMariaDBUser = 'root';
  cDefaultMariaDBPort = 3306;
  cDefaultOracleDatabase = 'bolddemo';
  cDefaultOracleUser = 'bolduser';

  // SQL Queries for database admin operations
  cSQLOracleCheckUser = 'SELECT USERNAME FROM ALL_USERS WHERE UPPER(USERNAME) = UPPER(''%s'')';
  cSQLOracleCreateUser = 'CREATE USER %s IDENTIFIED BY %s DEFAULT TABLESPACE USERS TEMPORARY TABLESPACE TEMP QUOTA UNLIMITED ON USERS';
  cSQLOracleGrantUser = 'GRANT CONNECT, RESOURCE, CREATE TABLE, CREATE SEQUENCE, CREATE VIEW TO %s';
  cSQLOracleDropUser = 'DROP USER %s CASCADE';
  cSQLMSSQLDropDb = 'DROP DATABASE [%s]';
  cSQLPGDropDb = 'DROP DATABASE "%s"';
  cSQLMariaDBDropDb = 'DROP DATABASE `%s`';

  // Misc
  cSharedFolder = 'Shared';
  cModelFileName = 'DemoModel.bld';
{$ENDREGION}

type
  TPersistenceType = (ptFireDAC, {$IFDEF UNIDAC}ptUniDAC,{$ENDIF} ptXML);
  TDatabaseType = (dtUnknown, dtMSSQL, dtPostgreSQL, dtFirebird, dtSQLite, dtMariaDB, dtOracle);

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
    procedure ConfigureMariaDB(AIni: TIniFile);
    procedure ConfigureOracle(AIni: TIniFile);
    procedure ConfigureXML(AIni: TIniFile);
    function OracleUserHasTable(const TableName: string): Boolean;
    procedure DropOracleSchemaTables;
  public
    procedure CreateDatabaseSchema;
    procedure CreateDatabaseIfNotExists;
    procedure DropDatabase;
    function DatabaseExists: Boolean;
    procedure ResetOracleSchema;
    procedure ResetOracleSchemaWithConfirm(AskConfirmation: Boolean);
    function IsOracleSchemaError(const ErrorMessage: string): Boolean;
    function HandleOracleSchemaError(const ErrorMessage: string): Boolean;
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
  case IndexText(AType, [cDbTypeMSSQL, cDbTypePostgreSQL, cDbTypeFirebird, cDbTypeSQLite, cDbTypeMariaDB, cDbTypeOracle]) of
    0: Result := dtMSSQL;
    1: Result := dtPostgreSQL;
    2: Result := dtFirebird;
    3: Result := dtSQLite;
    4: Result := dtMariaDB;
    5: Result := dtOracle;
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
    dtMariaDB: Result := 'MariaDB';
    dtOracle: Result := 'Oracle';
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
  // Close the Bold system first to release all database resources
  CloseSystem;

  // Deactivate persistence handles to release database resources
  if Assigned(FPersistenceHandleDB) then
  begin
    FPersistenceHandleDB.Active := False;
    FPersistenceHandleDB.DatabaseAdapter := nil;
  end;

  // Free adapters first (they hold internal connection wrappers with cached queries)
  FreeAndNil(FFireDACAdapter);
  {$IFDEF UNIDAC}
  FreeAndNil(FUniDACAdapter);
  {$ENDIF}

  // Disconnect database connections
  if Assigned(FFDConnection) and FFDConnection.Connected then
    FFDConnection.Connected := False;
  {$IFDEF UNIDAC}
  if Assigned(FUniConnection) and FUniConnection.Connected then
    FUniConnection.Connected := False;
  {$ENDIF}

  // Free remaining components
  FreeAndNil(FFDConnection);
  {$IFDEF UNIDAC}
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
        dtMariaDB: ConfigureMariaDB(Ini);
        dtOracle: ConfigureOracle(Ini);
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
      end;
    {$IFDEF UNIDAC}
    ptUniDAC:
      begin
        FUniConnection.ProviderName := cProviderInterBase;
        FUniConnection.Database := Database;
        FUniConnection.Username := User;
        FUniConnection.Password := Password;
        FUniDACAdapter.DatabaseEngine := dbeInterbaseSQLDialect3;
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
      end;
    {$IFDEF UNIDAC}
    ptUniDAC:
      begin
        FUniConnection.ProviderName := cProviderSQLite;
        FUniConnection.Database := Database;
        FUniDACAdapter.DatabaseEngine := dbeGenericANSISQL92;
      end;
    {$ENDIF}
  end;
end;

procedure TDemoDataModule.ConfigureMariaDB(AIni: TIniFile);
var
  Server, Database, User, Password, VendorLib: string;
  Port: Integer;
begin
  Server := AIni.ReadString(cSectionMariaDB, cKeyServer, cDefaultServer);
  Port := AIni.ReadInteger(cSectionMariaDB, cKeyPort, cDefaultMariaDBPort);
  Database := AIni.ReadString(cSectionMariaDB, cKeyDatabase, cDefaultMariaDBDatabase);
  User := AIni.ReadString(cSectionMariaDB, cKeyUser, cDefaultMariaDBUser);
  Password := AIni.ReadString(cSectionMariaDB, cKeyPassword, '');
  VendorLib := AIni.ReadString(cSectionMariaDB, cKeyVendorLib, '');

  case FPersistenceType of
    ptFireDAC:
      begin
        FFDConnection.Params.Clear;
        FFDConnection.Params.Add(cParamDriverID + '=' + cDriverMySQL);
        FFDConnection.Params.Add(cParamServer + '=' + Server);
        FFDConnection.Params.Add(cParamPort + '=' + IntToStr(Port));
        FFDConnection.Params.Add(cParamDatabase + '=' + Database);
        FFDConnection.Params.Add(cParamUserName + '=' + User);
        FFDConnection.Params.Add(cParamPassword + '=' + Password);
        if VendorLib <> '' then
          FFDConnection.Params.Add(cParamVendorLib + '=' + VendorLib);
        FFireDACAdapter.DatabaseEngine := dbeMySQL;
      end;
    {$IFDEF UNIDAC}
    ptUniDAC:
      begin
        FUniConnection.ProviderName := 'MySQL';
        FUniConnection.Server := Server;
        FUniConnection.Port := Port;
        FUniConnection.Database := Database;
        FUniConnection.Username := User;
        FUniConnection.Password := Password;
        FUniDACAdapter.DatabaseEngine := dbeMySQL;
      end;
    {$ENDIF}
  end;
end;

procedure TDemoDataModule.ConfigureOracle(AIni: TIniFile);
var
  Database, User, Password, VendorLib: string;
begin
  Database := AIni.ReadString(cSectionOracle, cKeyDatabase, cDefaultOracleDatabase);
  User := AIni.ReadString(cSectionOracle, cKeyUser, cDefaultOracleUser);
  Password := AIni.ReadString(cSectionOracle, cKeyPassword, '');
  VendorLib := AIni.ReadString(cSectionOracle, cKeyVendorLib, '');

  case FPersistenceType of
    ptFireDAC:
      begin
        FFDConnection.Params.Clear;
        FFDConnection.Params.Add(cParamDriverID + '=' + cDriverOracle);
        FFDConnection.Params.Add(cParamDatabase + '=' + Database);
        FFDConnection.Params.Add(cParamUserName + '=' + User);
        FFDConnection.Params.Add(cParamPassword + '=' + Password);
        if VendorLib <> '' then
          FFDConnection.Params.Add(cParamVendorLib + '=' + VendorLib);
        FFireDACAdapter.DatabaseEngine := dbeOracle;
      end;
    {$IFDEF UNIDAC}
    ptUniDAC:
      begin
        FUniConnection.ProviderName := 'Oracle';
        FUniConnection.Database := Database;
        FUniConnection.Username := User;
        FUniConnection.Password := Password;
        FUniDACAdapter.DatabaseEngine := dbeOracle;
      end;
    {$ENDIF}
  end;
end;

function TDemoDataModule.OracleUserHasTable(const TableName: string): Boolean;
var
  Ini: TIniFile;
  TempConn: TFDConnection;
  Query: TFDQuery;
  AdminUser, AdminPassword, Database, OracleUser, VendorLib: string;
begin
  Result := False;
  Ini := TIniFile.Create(FConfigFile);
  try
    AdminUser := Ini.ReadString(cSectionOracle, cKeyAdminUser, '');
    if AdminUser = '' then
      Exit;  // No admin credentials configured

    AdminPassword := Ini.ReadString(cSectionOracle, cKeyAdminPassword, '');
    Database := Ini.ReadString(cSectionOracle, cKeyDatabase, '');
    OracleUser := Ini.ReadString(cSectionOracle, cKeyUser, cDefaultOracleUser);
    VendorLib := Ini.ReadString(cSectionOracle, cKeyVendorLib, '');

    TempConn := TFDConnection.Create(nil);
    Query := TFDQuery.Create(nil);
    try
      TempConn.LoginPrompt := False;
      TempConn.Params.Clear;
      TempConn.Params.Add(cParamDriverID + '=' + cDriverOracle);
      TempConn.Params.Add(cParamDatabase + '=' + Database);
      TempConn.Params.Add(cParamUserName + '=' + AdminUser);
      TempConn.Params.Add(cParamPassword + '=' + AdminPassword);
      if VendorLib <> '' then
        TempConn.Params.Add(cParamVendorLib + '=' + VendorLib);
      try
        TempConn.Connected := True;
        Query.Connection := TempConn;
        Query.SQL.Text := 'SELECT TABLE_NAME FROM ALL_TABLES WHERE OWNER = UPPER(''' +
          OracleUser + ''') AND TABLE_NAME = UPPER(''' + TableName + ''')';
        Query.Open;
        Result := not Query.IsEmpty;
        Query.Close;
      except
        Result := False;
      end;
    finally
      Query.Free;
      TempConn.Free;
    end;
  finally
    Ini.Free;
  end;
end;

function TDemoDataModule.DatabaseExists: Boolean;
begin
  // XML persistence always "exists"
  if FPersistenceType = ptXML then
    Exit(True);

  // For file-based databases, check if file exists
  if FDatabaseType in [dtFirebird, dtSQLite] then
  begin
    Exit(FileExists(GetDatabaseName));
  end;

  // For Oracle, we must use admin credentials because we can't connect as
  // a user that doesn't exist yet. Check if user exists and has BOLD_TYPE table.
  if FDatabaseType = dtOracle then
  begin
    Exit(OracleUserHasTable('BOLD_TYPE'));
  end;

  // For other server-based databases (MSSQL, PostgreSQL, MySQL, MariaDB),
  // use Bold's built-in TableExists check.
  try
    case FPersistenceType of
      ptFireDAC:
        Result := FFireDACAdapter.DatabaseInterface.TableExists('BOLD_TYPE');
      {$IFDEF UNIDAC}
      ptUniDAC:
        Result := FUniDACAdapter.DatabaseInterface.TableExists('BOLD_TYPE');
      {$ENDIF}
    else
      Result := False;
    end;
  except
    Result := False;
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
      dtOracle:
        begin
          // Create Oracle user/schema if it doesn't exist
          // Uses AdminUser/AdminPassword to connect and create the target user
          var OracleUser := Ini.ReadString(cSectionOracle, cKeyUser, cDefaultOracleUser);
          var OraclePassword := Ini.ReadString(cSectionOracle, cKeyPassword, '');
          var AdminUser := Ini.ReadString(cSectionOracle, cKeyAdminUser, '');
          var AdminPassword := Ini.ReadString(cSectionOracle, cKeyAdminPassword, '');

          // If no admin credentials, skip automatic user creation
          if AdminUser = '' then
            Exit;

          var TempConn := TFDConnection.Create(nil);
          var Query := TFDQuery.Create(nil);
          try
            TempConn.LoginPrompt := False;
            TempConn.Params.Clear;
            TempConn.Params.Add(cParamDriverID + '=' + cDriverOracle);
            TempConn.Params.Add(cParamDatabase + '=' + Ini.ReadString(cSectionOracle, cKeyDatabase, cDefaultOracleDatabase));
            TempConn.Params.Add(cParamUserName + '=' + AdminUser);
            TempConn.Params.Add(cParamPassword + '=' + AdminPassword);
            var VendorLib := Ini.ReadString(cSectionOracle, cKeyVendorLib, '');
            if VendorLib <> '' then
              TempConn.Params.Add(cParamVendorLib + '=' + VendorLib);
            TempConn.Connected := True;
            Query.Connection := TempConn;
            // Check if user exists
            Query.SQL.Text := Format(cSQLOracleCheckUser, [OracleUser]);
            Query.Open;
            if Query.IsEmpty then
            begin
              Query.Close;
              // Create the user
              TempConn.ExecSQL(Format(cSQLOracleCreateUser, [OracleUser, OraclePassword]));
              // Grant necessary privileges
              TempConn.ExecSQL(Format(cSQLOracleGrantUser, [OracleUser]));
            end;
            Query.Close;
            TempConn.Connected := False;
          finally
            Query.Free;
            TempConn.Free;
          end;
        end;
      dtMSSQL, dtPostgreSQL, dtMariaDB:
        begin
          // MSSQL/PostgreSQL/MariaDB - use Bold's built-in CreateDatabase method
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

procedure TDemoDataModule.DropOracleSchemaTables;
var
  Ini: TIniFile;
  TempConn: TFDConnection;
  Query: TFDQuery;
  TableNames: TStringList;
  AdminUser, AdminPassword, Database, OracleUser, VendorLib: string;
  i: Integer;
begin
  Ini := TIniFile.Create(FConfigFile);
  TableNames := TStringList.Create;
  try
    AdminUser := Ini.ReadString(cSectionOracle, cKeyAdminUser, '');
    if AdminUser = '' then
      raise Exception.Create('Admin credentials required to reset Oracle schema');

    AdminPassword := Ini.ReadString(cSectionOracle, cKeyAdminPassword, '');
    Database := Ini.ReadString(cSectionOracle, cKeyDatabase, '');
    OracleUser := Ini.ReadString(cSectionOracle, cKeyUser, cDefaultOracleUser);
    VendorLib := Ini.ReadString(cSectionOracle, cKeyVendorLib, '');

    // Disconnect main connection if connected
    if Assigned(FFDConnection) and FFDConnection.Connected then
      FFDConnection.Connected := False;

    TempConn := TFDConnection.Create(nil);
    Query := TFDQuery.Create(nil);
    try
      TempConn.LoginPrompt := False;
      TempConn.Params.Clear;
      TempConn.Params.Add(cParamDriverID + '=' + cDriverOracle);
      TempConn.Params.Add(cParamDatabase + '=' + Database);
      TempConn.Params.Add(cParamUserName + '=' + AdminUser);
      TempConn.Params.Add(cParamPassword + '=' + AdminPassword);
      if VendorLib <> '' then
        TempConn.Params.Add(cParamVendorLib + '=' + VendorLib);
      TempConn.Connected := True;
      Query.Connection := TempConn;

      // Get all table names for the Oracle user
      Query.SQL.Text := 'SELECT TABLE_NAME FROM ALL_TABLES WHERE OWNER = UPPER(''' + OracleUser + ''')';
      Query.Open;
      while not Query.Eof do
      begin
        TableNames.Add(Query.FieldByName('TABLE_NAME').AsString);
        Query.Next;
      end;
      Query.Close;

      // Drop each table with CASCADE CONSTRAINTS and PURGE (to avoid recyclebin)
      for i := 0 to TableNames.Count - 1 do
      begin
        TempConn.ExecSQL('DROP TABLE ' + UpperCase(OracleUser) + '.' + TableNames[i] + ' CASCADE CONSTRAINTS PURGE');
      end;

      TempConn.Connected := False;
    finally
      Query.Free;
      TempConn.Free;
    end;
  finally
    TableNames.Free;
    Ini.Free;
  end;
end;

procedure TDemoDataModule.ResetOracleSchema;
begin
  ResetOracleSchemaWithConfirm(True);
end;

procedure TDemoDataModule.ResetOracleSchemaWithConfirm(AskConfirmation: Boolean);
begin
  if FDatabaseType <> dtOracle then
  begin
    MessageDlg('This function is only for Oracle databases.', mtInformation, [mbOK], 0);
    Exit;
  end;

  if AskConfirmation then
  begin
    if MessageDlg('This will drop ALL tables in the Oracle schema and recreate the Bold schema.' + sLineBreak + sLineBreak +
                  'All data will be lost!' + sLineBreak + sLineBreak +
                  'Do you want to continue?',
                  mtWarning, [mbYes, mbNo], 0) <> mrYes then
      Exit;
  end;

  try
    Screen.Cursor := crHourGlass;
    try
      // Deactivate system if active, discarding any dirty objects
      if BoldSystemHandle1.Active then
      begin
        if BoldSystemHandle1.System.BoldDirty then
          BoldSystemHandle1.System.Discard;
        BoldSystemHandle1.Active := False;
      end;

      // Drop all existing tables
      DropOracleSchemaTables;

      // Recreate the Bold schema
      CreateDatabaseSchema;

      if AskConfirmation then
        MessageDlg('Oracle schema has been reset successfully.' + sLineBreak +
                   'The Bold tables have been recreated.',
                   mtInformation, [mbOK], 0);
    finally
      Screen.Cursor := crDefault;
    end;
  except
    on E: Exception do
      MessageDlg('Error resetting Oracle schema: ' + E.Message, mtError, [mbOK], 0);
  end;
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

  // Close the Bold system first, discarding any dirty objects
  if BoldSystemHandle1.Active then
  begin
    if BoldSystemHandle1.System.BoldDirty then
      BoldSystemHandle1.System.Discard;
    BoldSystemHandle1.Active := False;
  end;

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
              dtMariaDB:
                begin
                  DatabaseName := Ini.ReadString(cSectionMariaDB, cKeyDatabase, cDefaultMariaDBDatabase);
                  TempFDConn.Params.Clear;
                  TempFDConn.Params.Add(cParamDriverID + '=' + cDriverMySQL);
                  TempFDConn.Params.Add(cParamServer + '=' + Ini.ReadString(cSectionMariaDB, cKeyServer, cDefaultServer));
                  TempFDConn.Params.Add(cParamPort + '=' + Ini.ReadString(cSectionMariaDB, cKeyPort, IntToStr(cDefaultMariaDBPort)));
                  TempFDConn.Params.Add(cParamUserName + '=' + Ini.ReadString(cSectionMariaDB, cKeyUser, cDefaultMariaDBUser));
                  TempFDConn.Params.Add(cParamPassword + '=' + Ini.ReadString(cSectionMariaDB, cKeyPassword, ''));
                  TempFDConn.Connected := True;
                  TempFDConn.ExecSQL(Format(cSQLMariaDBDropDb, [DatabaseName]));
                  TempFDConn.Connected := False;
                end;
              dtOracle:
                begin
                  // Drop all tables in Oracle schema (can't drop user without DBA privileges)
                  var OracleUser := Ini.ReadString(cSectionOracle, cKeyUser, cDefaultOracleUser);
                  TempFDConn.Params.Clear;
                  TempFDConn.Params.Add(cParamDriverID + '=' + cDriverOracle);
                  TempFDConn.Params.Add(cParamDatabase + '=' + Ini.ReadString(cSectionOracle, cKeyDatabase, cDefaultOracleDatabase));
                  TempFDConn.Params.Add(cParamUserName + '=' + OracleUser);
                  TempFDConn.Params.Add(cParamPassword + '=' + Ini.ReadString(cSectionOracle, cKeyPassword, ''));
                  var VendorLib := Ini.ReadString(cSectionOracle, cKeyVendorLib, '');
                  if VendorLib <> '' then
                    TempFDConn.Params.Add(cParamVendorLib + '=' + VendorLib);
                  TempFDConn.Connected := True;
                  // Get all table names and drop them
                  var TableNames := TStringList.Create;
                  try
                    var Query := TFDQuery.Create(nil);
                    try
                      Query.Connection := TempFDConn;
                      Query.SQL.Text := 'SELECT TABLE_NAME FROM USER_TABLES';
                      Query.Open;
                      while not Query.Eof do
                      begin
                        TableNames.Add(Query.FieldByName('TABLE_NAME').AsString);
                        Query.Next;
                      end;
                      Query.Close;
                    finally
                      Query.Free;
                    end;
                    // Drop all tables
                    for var i := 0 to TableNames.Count - 1 do
                    begin
                      try
                        TempFDConn.ExecSQL('DROP TABLE ' + TableNames[i] + ' CASCADE CONSTRAINTS PURGE');
                      except
                        // Ignore errors - table might have been dropped by CASCADE
                      end;
                    end;
                  finally
                    TableNames.Free;
                  end;
                  TempFDConn.Connected := False;
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
              dtMariaDB:
                begin
                  DatabaseName := Ini.ReadString(cSectionMariaDB, cKeyDatabase, cDefaultMariaDBDatabase);
                  TempUniConn.ProviderName := 'MySQL';
                  TempUniConn.Server := Ini.ReadString(cSectionMariaDB, cKeyServer, cDefaultServer);
                  TempUniConn.Port := Ini.ReadInteger(cSectionMariaDB, cKeyPort, cDefaultMariaDBPort);
                  TempUniConn.Username := Ini.ReadString(cSectionMariaDB, cKeyUser, cDefaultMariaDBUser);
                  TempUniConn.Password := Ini.ReadString(cSectionMariaDB, cKeyPassword, '');
                  TempUniConn.Connected := True;
                  TempUniConn.ExecSQL(Format(cSQLMariaDBDropDb, [DatabaseName]));
                  TempUniConn.Connected := False;
                end;
              dtOracle:
                begin
                  // Drop all tables in Oracle schema (can't drop user without DBA privileges)
                  var OracleUser := Ini.ReadString(cSectionOracle, cKeyUser, cDefaultOracleUser);
                  TempUniConn.ProviderName := 'Oracle';
                  TempUniConn.Database := Ini.ReadString(cSectionOracle, cKeyDatabase, cDefaultOracleDatabase);
                  TempUniConn.Username := OracleUser;
                  TempUniConn.Password := Ini.ReadString(cSectionOracle, cKeyPassword, '');
                  TempUniConn.Connected := True;
                  // Get all table names and drop them
                  var TableNames := TStringList.Create;
                  try
                    var Query := TUniQuery.Create(nil);
                    try
                      Query.Connection := TempUniConn;
                      Query.SQL.Text := 'SELECT TABLE_NAME FROM USER_TABLES';
                      Query.Open;
                      while not Query.Eof do
                      begin
                        TableNames.Add(Query.FieldByName('TABLE_NAME').AsString);
                        Query.Next;
                      end;
                      Query.Close;
                    finally
                      Query.Free;
                    end;
                    // Drop all tables
                    for var i := 0 to TableNames.Count - 1 do
                    begin
                      try
                        TempUniConn.ExecSQL('DROP TABLE ' + TableNames[i] + ' CASCADE CONSTRAINTS PURGE');
                      except
                        // Ignore errors - table might have been dropped by CASCADE
                      end;
                    end;
                  finally
                    TableNames.Free;
                  end;
                  TempUniConn.Connected := False;
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

function TDemoDataModule.IsOracleSchemaError(const ErrorMessage: string): Boolean;
begin
  // Detect Oracle errors that indicate schema/constraint mismatch
  Result := (FDatabaseType = dtOracle) and
            (Pos('ORA-01400', ErrorMessage) > 0);  // Cannot insert NULL
end;

function TDemoDataModule.HandleOracleSchemaError(const ErrorMessage: string): Boolean;
begin
  Result := False;
  if not IsOracleSchemaError(ErrorMessage) then
    Exit;

  if MessageDlg('Oracle database error: ' + sLineBreak + sLineBreak +
                ErrorMessage + sLineBreak + sLineBreak +
                'This may be caused by a schema mismatch (table constraints don''t match the model).' + sLineBreak + sLineBreak +
                'Do you want to reset the Oracle schema?' + sLineBreak +
                '(This will DROP all tables and recreate them - all data will be lost)',
                mtWarning, [mbYes, mbNo], 0) = mrYes then
  begin
    ResetOracleSchemaWithConfirm(False);  // Already confirmed
    Result := True;
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
    end
    // For Oracle, just use existing schema - errors will be handled below
  end;

  try
    BoldSystemHandle1.Active := True;
  except
    on E: EBoldMissingID do
    begin
      // Schema mismatch - BOLD_TYPE table doesn't have entries for model classes
      if (FDatabaseType = dtOracle) and
         (MessageDlg('The Oracle schema exists but is out of sync with the model.' + sLineBreak + sLineBreak +
                     'This usually happens when:' + sLineBreak +
                     '- The model was changed after the schema was created' + sLineBreak +
                     '- The schema was created by a different application' + sLineBreak + sLineBreak +
                     'Do you want to reset the Oracle schema?' + sLineBreak +
                     '(This will DROP all tables and recreate them)',
                     mtWarning, [mbYes, mbNo], 0) = mrYes) then
      begin
        ResetOracleSchemaWithConfirm(False);  // Already confirmed
        // Try again after reset
        BoldSystemHandle1.Active := True;
      end
      else
        raise;  // Re-raise if not Oracle or user declined
    end;
    on E: Exception do
    begin
      // Check for Oracle schema errors
      if HandleOracleSchemaError(E.Message) then
      begin
        // Schema was reset, try again
        BoldSystemHandle1.Active := True;
      end
      else
        raise;
    end;
  end;
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
