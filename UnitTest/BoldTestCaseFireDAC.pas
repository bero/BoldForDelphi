unit BoldTestCaseFireDAC;

{******************************************************************************}
{                                                                              }
{  BoldTestCaseFireDAC - FireDAC persistence testing base class                }
{                                                                              }
{  Concrete implementation of TBoldTestCasePersistence using FireDAC with      }
{  SQLite in-memory database for fast, isolated unit tests.                    }
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
  BoldDatabaseAdapterFireDAC,
  FireDAC.Comp.Client,
  FireDAC.Stan.Def,
  FireDAC.Stan.Async,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef;

type
  /// <summary>
  /// Base class for Bold test cases using FireDAC with SQLite in-memory database.
  /// Inherit from this class for tests that need database persistence.
  /// </summary>
  [TestFixture]
  TBoldTestCaseFireDAC = class(TBoldTestCasePersistence)
  private
    function GetFDConnection: TFDConnection;
    function GetFireDACAdapter: TBoldDatabaseAdapterFireDAC;
  protected
    function CreateConnection: TCustomConnection; override;
    function CreateDatabaseAdapter(AConnection: TCustomConnection): TBoldAbstractDatabaseAdapter; override;

    /// <summary>
    /// Override to customize connection parameters (e.g., use file-based SQLite)
    /// </summary>
    procedure ConfigureConnection(AConnection: TFDConnection); virtual;

    property FDConnection: TFDConnection read GetFDConnection;
    property FireDACAdapter: TBoldDatabaseAdapterFireDAC read GetFireDACAdapter;
  end;

implementation

{ TBoldTestCaseFireDAC }

function TBoldTestCaseFireDAC.CreateConnection: TCustomConnection;
var
  LConnection: TFDConnection;
begin
  LConnection := TFDConnection.Create(nil);
  LConnection.DriverName := 'SQLite';
  LConnection.Params.Database := ':memory:';
  LConnection.LoginPrompt := False;

  // Allow subclasses to customize
  ConfigureConnection(LConnection);

  Result := LConnection;
end;

function TBoldTestCaseFireDAC.CreateDatabaseAdapter(AConnection: TCustomConnection): TBoldAbstractDatabaseAdapter;
var
  LAdapter: TBoldDatabaseAdapterFireDAC;
begin
  LAdapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  LAdapter.Connection := AConnection as TFDConnection;
  Result := LAdapter;
end;

procedure TBoldTestCaseFireDAC.ConfigureConnection(AConnection: TFDConnection);
begin
  // Override in subclasses to customize connection
  // Default: SQLite in-memory database (already configured)
end;

function TBoldTestCaseFireDAC.GetFDConnection: TFDConnection;
begin
  Result := Connection as TFDConnection;
end;

function TBoldTestCaseFireDAC.GetFireDACAdapter: TBoldDatabaseAdapterFireDAC;
begin
  Result := DatabaseAdapter as TBoldDatabaseAdapterFireDAC;
end;

end.
