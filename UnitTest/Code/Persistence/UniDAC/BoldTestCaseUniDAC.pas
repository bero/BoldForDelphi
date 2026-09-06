{$include bold.inc}
unit BoldTestCaseUniDAC;

{******************************************************************************}
{                                                                              }
{  BoldTestCaseUniDAC - UniDAC persistence testing base class                  }
{                                                                              }
{  Concrete implementation of TBoldTestCasePersistence using UniDAC.           }
{  Database configuration is read from UnitTest.ini via BoldTestDatabaseConfig.}
{  Only Engine=SQLServer is supported: the UniDAC installation used for the    }
{  DebugUniDAC build configuration ships no SQLite provider.                   }
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
  /// Base class for Bold test cases using UniDAC against the SQL Server
  /// configured in UnitTest.ini. Inherit from this class for tests that need
  /// database persistence via UniDAC.
  /// </summary>
  TBoldTestCaseUniDAC = class(TBoldTestCasePersistence)
  private
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
  end;

implementation

uses
  System.IniFiles,
  BoldSQLDatabaseConfig,
  BoldTestDatabaseConfig,
  SQLServerUniProvider;

{ TBoldTestCaseUniDAC }

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
    if not SameText(Engine, 'SQLServer') then
      raise Exception.CreateFmt('UniDAC tests support Engine=SQLServer only (configured: %s): ' +
        'the UniDAC installation used by the DebugUniDAC configuration has no SQLite provider', [Engine]);

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
