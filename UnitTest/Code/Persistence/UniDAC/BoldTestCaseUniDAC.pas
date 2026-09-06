{$include bold.inc}
unit BoldTestCaseUniDAC;

{******************************************************************************}
{                                                                              }
{  BoldTestCaseUniDAC - UniDAC persistence testing base class                  }
{                                                                              }
{  Concrete implementation of TBoldTestCasePersistence using UniDAC.           }
{  Database configuration is read from UnitTest.ini via BoldTestDatabaseConfig.}
{  Engine=SQLServer and Engine=SQLite are supported; SQLite needs a UniDAC       }
{  with the SQLite provider (the vendored 10.4 copy has none, UniDAC 11 has).   }
{                                                                              }
{  Requirements:                                                               }
{    - Build configuration DebugUniDAC (defines UniDAC)                         }
{    - UniDAC environment variable pointing to the UniDAC installation root    }
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
  BoldDatabaseAdapterUniDAC,
  Uni;

type
  /// <summary>
  /// Base class for Bold test cases using UniDAC against the engine configured
  /// in UnitTest.ini or BOLD_TEST_ENGINE (SQL Server or SQLite). Inherit from
  /// this class for tests that need database persistence via UniDAC.
  /// </summary>
  TBoldTestCaseUniDAC = class(TBoldTestCasePersistence)
  private
    { SQLite only: a shared-cache in-memory database lives as long as one
      connection to it is open, and Bold closes the fixture's connection after
      creating the schema (PMapper.CloseDataBase). FireDAC survives that through
      ResourceOptions.KeepConnection; UniDAC needs this extra connection held
      open for the lifetime of the test. }
    FMemoryDbAnchor: TUniConnection;
    function GetUniConnection: TUniConnection;
    function GetUniDACAdapter: TBoldDatabaseAdapterUniDAC;
  protected
    function CreateConnection: TCustomConnection; override;
    function CreateDatabaseAdapter(AConnection: TCustomConnection): TBoldAbstractDatabaseAdapter; override;
    /// <summary>
    /// Configures the connection and adapter from UnitTest.ini. Override to
    /// customize.
    /// </summary>
    procedure ConfigureConnection(AConnection: TUniConnection; AAdapter: TBoldDatabaseAdapterUniDAC); virtual;
    property UniConnection: TUniConnection read GetUniConnection;
    property UniDACAdapter: TBoldDatabaseAdapterUniDAC read GetUniDACAdapter;
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
  end;

implementation

uses
  System.IniFiles,
  BoldSQLDatabaseConfig,
  BoldTestDatabaseConfig,
  SQLServerUniProvider,
  SQLiteUniProvider;

{ TBoldTestCaseUniDAC }

procedure TBoldTestCaseUniDAC.TearDown;
begin
  inherited; // frees the fixture connection first
  FreeAndNil(FMemoryDbAnchor); // last connection: the in-memory database goes with it
end;

procedure TBoldTestCaseUniDAC.SetUp;
begin
  // Ensure the test database exists before opening the connection
  CreateTestDatabase;
  inherited;
end;

function TBoldTestCaseUniDAC.CreateConnection: TCustomConnection;
var
  LConnection: TUniConnection;
begin
  LConnection := TUniConnection.Create(nil);
  LConnection.LoginPrompt := False;
  // Configured in CreateDatabaseAdapter via ConfigureConnection
  Result := LConnection;
end;

function TBoldTestCaseUniDAC.CreateDatabaseAdapter(AConnection: TCustomConnection): TBoldAbstractDatabaseAdapter;
var
  LAdapter: TBoldDatabaseAdapterUniDAC;
begin
  LAdapter := TBoldDatabaseAdapterUniDAC.Create(nil);
  LAdapter.Connection := AConnection as TUniConnection;
  ConfigureConnection(AConnection as TUniConnection, LAdapter);
  Result := LAdapter;
end;

procedure TBoldTestCaseUniDAC.ConfigureConnection(AConnection: TUniConnection; AAdapter: TBoldDatabaseAdapterUniDAC);
var
  Ini: TIniFile;
  Engine, OSAuth: string;
begin
  Ini := TIniFile.Create(GetIniFilePath);
  try
    Engine := GetTestDatabaseEngine; // BOLD_TEST_ENGINE or UnitTest.ini
    if SameText(Engine, 'SQLite') then
    begin
      // Same shared-cache in-memory database as the FireDAC configuration, so every
      // connection Bold opens in this process (CreateAnotherDatabaseConnection) sees
      // the same tables. Direct = the embedded engine, no sqlite3.dll needed.
      AConnection.ProviderName := 'SQLite';
      AConnection.Database := Ini.ReadString('SQLite', 'Database', 'file:memdb1?mode=memory&cache=shared');
      AConnection.SpecificOptions.Values['Direct'] := 'True';
      AConnection.SpecificOptions.Values['EnableSharedCache'] := 'True';
      AConnection.SpecificOptions.Values['ForceCreateDatabase'] := 'True';
      AAdapter.DatabaseEngine := dbeGenericANSISQL92;
      with AAdapter.SQLDatabaseConfig do
      begin
        ColumnTypeForText := 'TEXT';
        ColumnTypeForUnicodeText := 'TEXT';
        ColumnTypeForAnsiText := 'TEXT';
        ColumnTypeForInt64 := 'INTEGER';
      end;
      FreeAndNil(FMemoryDbAnchor);
      FMemoryDbAnchor := TUniConnection.Create(nil);
      FMemoryDbAnchor.Assign(AConnection);
      FMemoryDbAnchor.LoginPrompt := False;
      FMemoryDbAnchor.Open;
      Exit;
    end;
    if not SameText(Engine, 'SQLServer') then
      raise Exception.CreateFmt('UniDAC tests support Engine=SQLServer or SQLite (configured: %s)', [Engine]);

    AConnection.ProviderName := 'SQL Server';
    AConnection.Server := Ini.ReadString('SQLServer', 'Server', '.\SQLEXPRESS');
    AConnection.Database := Ini.ReadString('SQLServer', 'Database', 'BoldUnitTest');
    OSAuth := Ini.ReadString('SQLServer', 'OSAuthentication', 'Yes');
    if SameText(OSAuth, 'Yes') or SameText(OSAuth, 'True') or (OSAuth = '1') then
      AConnection.SpecificOptions.Values['Authentication'] := 'auWindows'
    else
    begin
      AConnection.SpecificOptions.Values['Authentication'] := 'auServer';
      AConnection.Username := Ini.ReadString('SQLServer', 'User', '');
      AConnection.Password := Ini.ReadString('SQLServer', 'Password', '');
    end;
    AAdapter.DatabaseEngine := dbeSQLServer;
  finally
    Ini.Free;
  end;
end;

function TBoldTestCaseUniDAC.GetUniConnection: TUniConnection;
begin
  Result := Connection as TUniConnection;
end;

function TBoldTestCaseUniDAC.GetUniDACAdapter: TBoldDatabaseAdapterUniDAC;
begin
  Result := DatabaseAdapter as TBoldDatabaseAdapterUniDAC;
end;

end.
