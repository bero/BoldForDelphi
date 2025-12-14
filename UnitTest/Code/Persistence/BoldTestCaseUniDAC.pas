{$include bold.inc}
unit BoldTestCaseUniDAC;

{******************************************************************************}
{                                                                              }
{  BoldTestCaseUniDAC - UniDAC persistence testing base class                  }
{                                                                              }
{  Concrete implementation of TBoldTestCasePersistence using UniDAC with       }
{  SQLite in-memory database for fast, isolated unit tests.                    }
{                                                                              }
{  Requirements:                                                               }
{    - UniDAC must be installed                                                }
{    - UniDAC environment variable must point to UniDAC installation           }
{                                                                              }
{******************************************************************************}

{$IFDEF UniDAC}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Data.DB,
  BoldTestCasePersistence,
  BoldAbstractDatabaseAdapter,
  BoldDatabaseAdapterUniDAC,
  Uni,
  SQLiteUniProvider;

type
  /// <summary>
  /// Base class for Bold test cases using UniDAC with SQLite in-memory database.
  /// Inherit from this class for tests that need database persistence via UniDAC.
  /// </summary>
  [TestFixture]
  TBoldTestCaseUniDAC = class(TBoldTestCasePersistence)
  private
    function GetUniConnection: TUniConnection;
    function GetUniDACAdapter: TBoldDatabaseAdapterUniDAC;
  protected
    function CreateConnection: TCustomConnection; override;
    function CreateDatabaseAdapter(AConnection: TCustomConnection): TBoldAbstractDatabaseAdapter; override;

    /// <summary>
    /// Override to customize connection parameters (e.g., use file-based SQLite or different provider)
    /// </summary>
    procedure ConfigureConnection(AConnection: TUniConnection); virtual;

    property UniConnection: TUniConnection read GetUniConnection;
    property UniDACAdapter: TBoldDatabaseAdapterUniDAC read GetUniDACAdapter;
  end;

implementation

{ TBoldTestCaseUniDAC }

function TBoldTestCaseUniDAC.CreateConnection: TCustomConnection;
var
  LConnection: TUniConnection;
begin
  LConnection := TUniConnection.Create(nil);
  LConnection.ProviderName := 'SQLite';
  LConnection.Database := ':memory:';
  LConnection.LoginPrompt := False;

  // Allow subclasses to customize
  ConfigureConnection(LConnection);

  Result := LConnection;
end;

function TBoldTestCaseUniDAC.CreateDatabaseAdapter(AConnection: TCustomConnection): TBoldAbstractDatabaseAdapter;
var
  LAdapter: TBoldDatabaseAdapterUniDAC;
begin
  LAdapter := TBoldDatabaseAdapterUniDAC.Create(nil);
  LAdapter.Connection := AConnection as TUniConnection;
  Result := LAdapter;
end;

procedure TBoldTestCaseUniDAC.ConfigureConnection(AConnection: TUniConnection);
begin
  // Override in subclasses to customize connection
  // Default: SQLite in-memory database (already configured)
end;

function TBoldTestCaseUniDAC.GetUniConnection: TUniConnection;
begin
  Result := Connection as TUniConnection;
end;

function TBoldTestCaseUniDAC.GetUniDACAdapter: TBoldDatabaseAdapterUniDAC;
begin
  Result := DatabaseAdapter as TBoldDatabaseAdapterUniDAC;
end;

{$ELSE}

interface

implementation

{$ENDIF}

end.
