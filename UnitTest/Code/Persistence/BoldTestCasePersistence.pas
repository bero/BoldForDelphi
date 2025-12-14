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
{    2. Override ConfigureModel to set up a custom model (optional)            }
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
  BoldElements,
  BoldSystemRT,
  BoldSystemHandle,
  BoldHandles,
  BoldModel,
  BoldAbstractModel,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldAbstractPersistenceHandleDB,
  BoldAbstractDatabaseAdapter,
  BoldUndoHandler,
  BoldId;

const
  // Default test model: root class + TestClass with Name attribute
  DefaultTestModel: array[0..47] of string = (
    'VERSION 19',
    '(Model',
    #9'"DefaultTestModel"',
    #9'"BusinessClassesRoot"',
    #9'""',
    #9'""',
    #9'"_Boldify.boldified=True,Bold.DelphiName=<Name>,Bold.UnitName=DefaultTestModel,Bold.RootClass=BusinessClassesRoot"',
    #9'(Classes',
    #9#9'(Class',
    #9#9#9'"BusinessClassesRoot"',
    #9#9#9'"<NONE>"',
    #9#9#9'TRUE',
    #9#9#9'FALSE',
    #9#9#9'""',
    #9#9#9'""',
    #9#9#9'"_Boldify.autoCreated=True,persistence=persistent,Bold.TableName=<Prefix>_OBJECT"',
    #9#9#9'(Attributes',
    #9#9#9')',
    #9#9#9'(Methods',
    #9#9#9')',
    #9#9')',
    #9#9'(Class',
    #9#9#9'"TestClass"',
    #9#9#9'"BusinessClassesRoot"',
    #9#9#9'TRUE',
    #9#9#9'FALSE',
    #9#9#9'""',
    #9#9#9'""',
    #9#9#9'"persistence=persistent"',
    #9#9#9'(Attributes',
    #9#9#9#9'(Attribute',
    #9#9#9#9#9'"Name"',
    #9#9#9#9#9'"String"',
    #9#9#9#9#9'FALSE',
    #9#9#9#9#9'""',
    #9#9#9#9#9'""',
    #9#9#9#9#9'2',
    #9#9#9#9#9'""',
    #9#9#9#9#9'"persistence=Persistent"',
    #9#9#9#9')',
    #9#9#9')',
    #9#9#9'(Methods',
    #9#9#9')',
    #9#9')',
    #9')',
    #9'(Associations',
    #9')',
    ')'
  );

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
    /// Configure the model. Default loads DefaultTestModel.
    /// Override to use a custom model.
    /// </summary>
    procedure ConfigureModel; virtual;

    /// <summary>
    /// Override this to register business classes before system activation.
    /// </summary>
    procedure RegisterBusinessClasses; virtual;

    /// <summary>
    /// Load model from string array.
    /// </summary>
    procedure LoadModelFromStrings(const AModelStrings: array of string);

    // Database operations
    procedure UpdateDatabase;
    procedure DiscardChanges;
    procedure RefreshSystem;

    // Helper methods for creating and finding objects
    function GetClassTypeInfo(const AClassName: string): TBoldClassTypeInfo;
    function CreateObject(const AClassName: string): TBoldObject;
    function FindObjectById(AObjectId: TBoldObjectId): TBoldObject;

    // Helper methods for attributes
    function GetAttributeAsString(AObject: TBoldObject; const AAttributeName: string): string;
    procedure SetAttributeAsString(AObject: TBoldObject; const AAttributeName: string; const AValue: string);
    function GetAttributeAsInteger(AObject: TBoldObject; const AAttributeName: string): Integer;
    procedure SetAttributeAsInteger(AObject: TBoldObject; const AAttributeName: string; AValue: Integer);

    // Assertion helpers
    procedure AssertAttributeEquals(AObject: TBoldObject; const AAttributeName: string;
      const AExpectedValue: string; const AMessage: string = '');
    procedure AssertAttributeEqualsInt(AObject: TBoldObject; const AAttributeName: string;
      AExpectedValue: Integer; const AMessage: string = '');
    procedure AssertObjectIsPersistent(AObject: TBoldObject; const AMessage: string = '');
    procedure AssertObjectIsNew(AObject: TBoldObject; const AMessage: string = '');

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
  // Load default test model
  LoadModelFromStrings(DefaultTestModel);
end;

procedure TBoldTestCasePersistence.LoadModelFromStrings(const AModelStrings: array of string);
var
  ModelStrings: TStringList;
  i: Integer;
begin
  ModelStrings := TStringList.Create;
  try
    for i := Low(AModelStrings) to High(AModelStrings) do
      ModelStrings.Add(AModelStrings[i]);
    Model.SetFromModelAsString(ModelStrings);
  finally
    ModelStrings.Free;
  end;
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

function TBoldTestCasePersistence.GetClassTypeInfo(const AClassName: string): TBoldClassTypeInfo;
begin
  Result := System.BoldSystemTypeInfo.ClassTypeInfoByExpressionName[AClassName];
end;

function TBoldTestCasePersistence.CreateObject(const AClassName: string): TBoldObject;
var
  ClassTypeInfo: TBoldClassTypeInfo;
begin
  ClassTypeInfo := GetClassTypeInfo(AClassName);
  Assert.IsNotNull(ClassTypeInfo, Format('Class "%s" not found in model', [AClassName]));
  Result := TBoldObject.InternalCreateNewWithClassAndSystem(ClassTypeInfo, System, True);
end;

function TBoldTestCasePersistence.FindObjectById(AObjectId: TBoldObjectId): TBoldObject;
var
  Locator: TBoldObjectLocator;
begin
  Result := nil;
  Locator := System.EnsuredLocatorByID[AObjectId];
  if Assigned(Locator) then
    Result := Locator.BoldObject;
end;

function TBoldTestCasePersistence.GetAttributeAsString(AObject: TBoldObject;
  const AAttributeName: string): string;
begin
  Result := AObject.BoldMemberByExpressionName[AAttributeName].AsString;
end;

procedure TBoldTestCasePersistence.SetAttributeAsString(AObject: TBoldObject;
  const AAttributeName: string; const AValue: string);
begin
  AObject.BoldMemberByExpressionName[AAttributeName].AsString := AValue;
end;

function TBoldTestCasePersistence.GetAttributeAsInteger(AObject: TBoldObject;
  const AAttributeName: string): Integer;
begin
  Result := StrToInt(AObject.BoldMemberByExpressionName[AAttributeName].AsString);
end;

procedure TBoldTestCasePersistence.SetAttributeAsInteger(AObject: TBoldObject;
  const AAttributeName: string; AValue: Integer);
begin
  AObject.BoldMemberByExpressionName[AAttributeName].AsString := IntToStr(AValue);
end;

procedure TBoldTestCasePersistence.AssertAttributeEquals(AObject: TBoldObject;
  const AAttributeName: string; const AExpectedValue: string; const AMessage: string);
var
  ActualValue: string;
  Msg: string;
begin
  ActualValue := GetAttributeAsString(AObject, AAttributeName);
  if AMessage <> '' then
    Msg := AMessage
  else
    Msg := Format('Attribute "%s" expected "%s" but was "%s"', [AAttributeName, AExpectedValue, ActualValue]);
  Assert.AreEqual(AExpectedValue, ActualValue, Msg);
end;

procedure TBoldTestCasePersistence.AssertAttributeEqualsInt(AObject: TBoldObject;
  const AAttributeName: string; AExpectedValue: Integer; const AMessage: string);
var
  ActualValue: Integer;
  Msg: string;
begin
  ActualValue := GetAttributeAsInteger(AObject, AAttributeName);
  if AMessage <> '' then
    Msg := AMessage
  else
    Msg := Format('Attribute "%s" expected %d but was %d', [AAttributeName, AExpectedValue, ActualValue]);
  Assert.AreEqual(AExpectedValue, ActualValue, Msg);
end;

procedure TBoldTestCasePersistence.AssertObjectIsPersistent(AObject: TBoldObject;
  const AMessage: string);
var
  Msg: string;
begin
  if AMessage <> '' then
    Msg := AMessage
  else
    Msg := 'Object should be persistent';
  Assert.IsTrue(AObject.BoldPersistenceState = bvpsCurrent, Msg);
end;

procedure TBoldTestCasePersistence.AssertObjectIsNew(AObject: TBoldObject;
  const AMessage: string);
var
  Msg: string;
begin
  if AMessage <> '' then
    Msg := AMessage
  else
    Msg := 'Object should be new (not yet persisted)';
  Assert.IsTrue(AObject.BoldPersistenceState = bvpsModified, Msg);
end;

end.
