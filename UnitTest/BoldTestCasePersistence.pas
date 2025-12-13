unit BoldTestCasePersistence;

{******************************************************************************}
{                                                                              }
{  BoldTestCasePersistence - Abstract base class for persistence testing       }
{                                                                              }
{  This unit provides an abstract base class for Bold unit tests that need     }
{  database persistence. Concrete implementations provide adapter-specific     }
{  functionality for FireDAC, UniDAC, or other database access libraries.      }
{                                                                              }
{  Usage:                                                                      }
{    1. Inherit from TBoldTestCaseFireDAC or TBoldTestCaseUniDAC               }
{    2. Override ConfigureModel to set up your test model                      }
{    3. Override RegisterBusinessClasses if using generated classes            }
{    4. Use System property to access the active BoldSystem                    }
{                                                                              }
{******************************************************************************}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Data.DB,
  BoldSystem,
  BoldSystemHandle,
  BoldHandles,
  BoldModel,
  BoldAbstractModel,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldAbstractPersistenceHandleDB,
  BoldAbstractDatabaseAdapter,
  BoldUndoHandler;

type
  /// <summary>
  /// Abstract base class for Bold test cases that require database persistence.
  /// Subclasses must implement CreateConnection and CreateDatabaseAdapter to
  /// provide the specific database access library (FireDAC, UniDAC, etc.).
  /// </summary>
  [TestFixture]
  TBoldTestCasePersistence = class
  private
    FConnection: TCustomConnection;
    FDatabaseAdapter: TBoldAbstractDatabaseAdapter;
    FPersistenceHandle: TBoldPersistenceHandleDB;
    FSystemHandle: TBoldSystemHandle;
    FModel: TBoldModel;
    FSystemTypeInfoHandle: TBoldSystemTypeInfoHandle;
  protected
    function GetSystem: TBoldSystem;
    function GetUndoHandler: TBoldUndoHandler;

    /// <summary>
    /// Create and configure the database connection.
    /// Must return a connected (or ready to connect) TCustomConnection descendant.
    /// Default implementation creates SQLite in-memory database.
    /// </summary>
    function CreateConnection: TCustomConnection; virtual; abstract;

    /// <summary>
    /// Create the Bold database adapter for the specific library.
    /// Must configure the adapter with the provided connection.
    /// </summary>
    function CreateDatabaseAdapter(AConnection: TCustomConnection): TBoldAbstractDatabaseAdapter; virtual; abstract;

    /// <summary>
    /// Open the database connection. Override if library needs special handling.
    /// </summary>
    procedure OpenConnection; virtual;

    /// <summary>
    /// Close the database connection. Override if library needs special handling.
    /// </summary>
    procedure CloseConnection; virtual;

    /// <summary>
    /// Override this to configure the model programmatically.
    /// This is called before system activation.
    /// </summary>
    procedure ConfigureModel; virtual;

    /// <summary>
    /// Override this to register business classes before system activation.
    /// </summary>
    procedure RegisterBusinessClasses; virtual;

    procedure UpdateDatabase;
    procedure DiscardChanges;
    procedure RefreshSystem;

    property Connection: TCustomConnection read FConnection;
    property DatabaseAdapter: TBoldAbstractDatabaseAdapter read FDatabaseAdapter;
    property System: TBoldSystem read GetSystem;
    property UndoHandler: TBoldUndoHandler read GetUndoHandler;
    property SystemHandle: TBoldSystemHandle read FSystemHandle;
    property Model: TBoldModel read FModel;
  public
    [Setup]
    procedure SetUp; virtual;
    [TearDown]
    procedure TearDown; virtual;
  end;

implementation

uses
  BoldDefs;

{ TBoldTestCasePersistence }

procedure TBoldTestCasePersistence.SetUp;
begin
  // Create connection (adapter-specific)
  FConnection := CreateConnection;

  // Create Bold database adapter (adapter-specific)
  FDatabaseAdapter := CreateDatabaseAdapter(FConnection);

  // Create system type info handle
  FSystemTypeInfoHandle := TBoldSystemTypeInfoHandle.Create(nil);

  // Create and configure model
  FModel := TBoldModel.Create(nil);
  FSystemTypeInfoHandle.BoldModel := FModel;

  // Allow subclasses to configure the model
  ConfigureModel;

  // Create persistence handle
  FPersistenceHandle := TBoldPersistenceHandleDB.Create(nil);
  FPersistenceHandle.BoldModel := FModel;
  FPersistenceHandle.DatabaseAdapter := FDatabaseAdapter;

  // Allow subclasses to register business classes
  RegisterBusinessClasses;

  // Create system handle
  FSystemHandle := TBoldSystemHandle.Create(nil);
  FSystemHandle.SystemTypeInfoHandle := FSystemTypeInfoHandle;
  FSystemHandle.PersistenceHandle := FPersistenceHandle;

  // Create database schema and activate
  OpenConnection;
  FPersistenceHandle.CreateDataBaseSchema;
  FSystemHandle.Active := True;
end;

procedure TBoldTestCasePersistence.TearDown;
begin
  if Assigned(FSystemHandle) then
  begin
    if FSystemHandle.Active then
    begin
      if Assigned(FSystemHandle.System) then
        FSystemHandle.System.Discard;
      FSystemHandle.Active := False;
    end;
    FreeAndNil(FSystemHandle);
  end;

  FreeAndNil(FPersistenceHandle);
  FreeAndNil(FModel);
  FreeAndNil(FSystemTypeInfoHandle);
  FreeAndNil(FDatabaseAdapter);

  if Assigned(FConnection) then
  begin
    CloseConnection;
    FreeAndNil(FConnection);
  end;
end;

procedure TBoldTestCasePersistence.OpenConnection;
begin
  FConnection.Open;
end;

procedure TBoldTestCasePersistence.CloseConnection;
begin
  if FConnection.Connected then
    FConnection.Close;
end;

function TBoldTestCasePersistence.GetSystem: TBoldSystem;
begin
  Result := FSystemHandle.System;
end;

function TBoldTestCasePersistence.GetUndoHandler: TBoldUndoHandler;
begin
  if Assigned(System) and Assigned(System.UndoHandler) then
    Result := System.UndoHandler as TBoldUndoHandler
  else
    Result := nil;
end;

procedure TBoldTestCasePersistence.ConfigureModel;
begin
  // Override in subclasses to configure the model programmatically
end;

procedure TBoldTestCasePersistence.RegisterBusinessClasses;
begin
  // Override in subclasses to register generated business classes
end;

procedure TBoldTestCasePersistence.UpdateDatabase;
begin
  if Assigned(System) then
    FSystemHandle.UpdateDatabase;
end;

procedure TBoldTestCasePersistence.DiscardChanges;
begin
  if Assigned(System) then
    System.Discard;
end;

procedure TBoldTestCasePersistence.RefreshSystem;
begin
  if FSystemHandle.Active then
  begin
    System.Discard;
    FSystemHandle.Active := False;
    FSystemHandle.Active := True;
  end;
end;

end.
