unit BoldTestCaseDB;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  BoldTestCase,
  BoldSystem,
  BoldSystemHandle,
  BoldHandles,
  BoldModel,
  BoldAbstractModel,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldAbstractPersistenceHandleDB,
  BoldAbstractDatabaseAdapter,
  BoldDatabaseAdapterFireDAC,
  BoldUndoHandler,
  FireDAC.Comp.Client,
  FireDAC.Stan.Def,
  FireDAC.Stan.Async,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef;

type
  /// <summary>
  /// Base class for Bold test cases that require database persistence.
  /// Uses FireDAC with SQLite in-memory database for fast, isolated tests.
  /// </summary>
  [TestFixture]
  TBoldTestCaseDB = class(TBoldTestCase)
  private
    FConnection: TFDConnection;
    FDatabaseAdapter: TBoldDatabaseAdapterFireDAC;
    FPersistenceHandle: TBoldPersistenceHandleDB;
    FSystemHandle: TBoldSystemHandle;
    FModel: TBoldModel;
    FSystemTypeInfoHandle: TBoldSystemTypeInfoHandle;
  protected
    function GetSystem: TBoldSystem;
    function GetUndoHandler: TBoldUndoHandler;

    /// <summary>
    /// Override this to return the path to your model file (.bld or .xmi)
    /// </summary>
    function GetModelFilePath: string; virtual;

    /// <summary>
    /// Override this to configure the model programmatically if not using a file
    /// </summary>
    procedure ConfigureModel; virtual;

    /// <summary>
    /// Override this to register business classes before system activation
    /// </summary>
    procedure RegisterBusinessClasses; virtual;

    procedure SetUp; override;
    procedure TearDown; override;

    procedure UpdateDatabase;
    procedure DiscardChanges;
    procedure RefreshSystem;

    property System: TBoldSystem read GetSystem;
    property UndoHandler: TBoldUndoHandler read GetUndoHandler;
    property SystemHandle: TBoldSystemHandle read FSystemHandle;
    property Model: TBoldModel read FModel;
  end;

implementation

uses
  BoldDefs;

{ TBoldTestCaseDB }

procedure TBoldTestCaseDB.SetUp;
begin
  inherited;

  // Create FireDAC connection for SQLite in-memory database
  FConnection := TFDConnection.Create(nil);
  FConnection.DriverName := 'SQLite';
  FConnection.Params.Database := ':memory:';
  FConnection.LoginPrompt := False;

  // Create Bold database adapter
  FDatabaseAdapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  FDatabaseAdapter.Connection := FConnection;

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
  FConnection.Open;
  FPersistenceHandle.CreateDataBaseSchema;
  FSystemHandle.Active := True;
end;

procedure TBoldTestCaseDB.TearDown;
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
    if FConnection.Connected then
      FConnection.Close;
    FreeAndNil(FConnection);
  end;

  inherited;
end;

function TBoldTestCaseDB.GetSystem: TBoldSystem;
begin
  Result := FSystemHandle.System;
end;

function TBoldTestCaseDB.GetUndoHandler: TBoldUndoHandler;
begin
  if Assigned(System) and Assigned(System.UndoHandler) then
    Result := System.UndoHandler as TBoldUndoHandler
  else
    Result := nil;
end;

function TBoldTestCaseDB.GetModelFilePath: string;
begin
  // Override in subclasses to return model file path
  Result := '';
end;

procedure TBoldTestCaseDB.ConfigureModel;
var
  ModelPath: string;
begin
  // Load model from file if path is provided
  ModelPath := GetModelFilePath;
  if ModelPath <> '' then
  begin
    if not FileExists(ModelPath) then
      raise Exception.CreateFmt('Model file not found: %s', [ModelPath]);
    // Model loading from file requires BoldUMLModelDataModule
    // For unit tests, prefer programmatic model configuration
  end;
  // Otherwise, subclasses should configure the model programmatically
end;

procedure TBoldTestCaseDB.RegisterBusinessClasses;
begin
  // Override in subclasses to register generated business classes
end;

procedure TBoldTestCaseDB.UpdateDatabase;
begin
  if Assigned(System) then
    FSystemHandle.UpdateDatabase;
end;

procedure TBoldTestCaseDB.DiscardChanges;
begin
  if Assigned(System) then
    System.Discard;
end;

procedure TBoldTestCaseDB.RefreshSystem;
begin
  if FSystemHandle.Active then
  begin
    System.Discard;
    FSystemHandle.Active := False;
    FSystemHandle.Active := True;
  end;
end;

end.
