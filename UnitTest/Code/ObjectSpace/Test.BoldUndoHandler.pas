unit Test.BoldUndoHandler;

{ DUnitX tests for BoldUndoHandler - Undo/Redo functionality }

interface

uses
  SysUtils,
  Classes,
  DUnitX.TestFramework,
  BoldDefs,
  BoldSystem,
  BoldDomainElement,
  BoldAttributes,
  BoldId,
  BoldSubscription,
  BoldUndoHandler,
  BoldUndoInterfaces,
  BoldFreeStandingValues,
  BoldValueInterfaces,
  BoldValueSpaceInterfaces,
  BoldElements,
  UndoTestModelClasses;

type
  [TestFixture]
  [Category('UndoHandler')]
  TTestBoldUndoHandler = class
  private
    FUndoHandler: TBoldUndoHandler;
    FSomeClassList: TSomeClassList;
    function GetSystem: TBoldSystem;
    function GetUndoHandler: TBoldUndoHandler;
    procedure RefreshSystem;
    procedure UpdateDatabase;
    procedure SetupParentChildRelationships;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    // Basic undo handler tests
    [Test]
    [Category('Quick')]
    procedure TestUndoHandlerExists;

    [Test]
    [Category('Quick')]
    procedure TestUndoHandlerEnabled;

    [Test]
    [Category('Quick')]
    procedure TestSetCheckPoint;

    // Object creation undo tests
    [Test]
    [Category('Quick')]
    procedure TestNewObjectRecordedWithCorrectExistenceState;

    [Test]
    [Category('Quick')]
    procedure TestDeletedObjectRecordedWithCorrectExistenceState;

    // Attribute modification tests
    [Test]
    [Category('Quick')]
    procedure TestModifyAttributeRecordsOldValue;

    [Test]
    [Category('Quick')]
    procedure TestUndoRestoresAttributeValue;

    // Undo/Redo cycle tests
    [Test]
    [Category('Quick')]
    procedure TestUndoObjectCreation;

    [Test]
    [Category('Quick')]
    procedure TestRedoObjectCreation;

    property System: TBoldSystem read GetSystem;
    property UndoHandler: TBoldUndoHandler read GetUndoHandler;
  end;

implementation

uses
  maan_UndoRedoBase,
  maan_UndoRedoTestCaseUtils;

{ TTestBoldUndoHandler }

procedure TTestBoldUndoHandler.SetUp;
begin
  EnsureDM;
  Assert.IsNotNull(dmUndoRedo, 'dmUndoRedo should be created');
  Assert.IsNotNull(dmUndoRedo.BoldSystemHandle1.System, 'System should be active');

  FUndoHandler := dmUndoRedo.BoldSystemHandle1.System.UndoHandler as TBoldUndoHandler;
  FUndoHandler.Enabled := True;

  FSomeClassList := TSomeClassList.Create;
end;

procedure TTestBoldUndoHandler.TearDown;
begin
  if Assigned(dmUndoRedo) then
  begin
    if dmUndoRedo.BoldSystemHandle1.Active then
    begin
      dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
      dmUndoRedo.BoldSystemHandle1.Active := False;
    end;
    FreeAndNil(dmUndoRedo);
  end;
  FreeAndNil(FSomeClassList);
end;

function TTestBoldUndoHandler.GetSystem: TBoldSystem;
begin
  Result := dmUndoRedo.BoldSystemHandle1.System;
end;

function TTestBoldUndoHandler.GetUndoHandler: TBoldUndoHandler;
begin
  Result := FUndoHandler;
end;

procedure TTestBoldUndoHandler.RefreshSystem;
begin
  UpdateDatabase;
  dmUndoRedo.BoldSystemHandle1.Active := False;
  FSomeClassList.Clear;
  dmUndoRedo.BoldSystemHandle1.Active := True;
  FUndoHandler := dmUndoRedo.BoldSystemHandle1.System.UndoHandler as TBoldUndoHandler;
  FUndoHandler.Enabled := True;
  FetchClass(System, FSomeClassList, TSomeClass);
end;

procedure TTestBoldUndoHandler.UpdateDatabase;
begin
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
end;

procedure TTestBoldUndoHandler.SetupParentChildRelationships;
begin
  GenerateObjects(System, 'SomeClass', 4);
  UpdateDatabase;
  FetchClass(System, FSomeClassList, TSomeClass);
  FSomeClassList[1].parent := FSomeClassList[0];
  FSomeClassList[3].parent := FSomeClassList[2];
end;

// Basic undo handler tests

procedure TTestBoldUndoHandler.TestUndoHandlerExists;
begin
  Assert.IsNotNull(System.UndoHandler, 'UndoHandler should exist on system');
  Assert.IsTrue(System.UndoHandler is TBoldUndoHandler, 'UndoHandler should be TBoldUndoHandler');
end;

procedure TTestBoldUndoHandler.TestUndoHandlerEnabled;
begin
  UndoHandler.Enabled := True;
  Assert.IsTrue(UndoHandler.Enabled, 'UndoHandler should be enabled');

  UndoHandler.Enabled := False;
  Assert.IsFalse(UndoHandler.Enabled, 'UndoHandler should be disabled');

  UndoHandler.Enabled := True; // Re-enable for other tests
end;

procedure TTestBoldUndoHandler.TestSetCheckPoint;
var
  CheckPointName: string;
begin
  CheckPointName := UndoHandler.SetCheckPoint('TestCheckPoint');
  Assert.IsNotEmpty(CheckPointName, 'CheckPoint name should not be empty');
end;

// Object creation undo tests

procedure TTestBoldUndoHandler.TestNewObjectRecordedWithCorrectExistenceState;
var
  NewObject: TSomeClass;
  oc: TBoldFreeStandingObjectContents;
begin
  // Create a new persistent object
  NewObject := CreateSomeClass(System, nil, True);

  // Get the object contents from the undo block
  oc := UndoHandler.UndoBlocks.CurrentBlock.FSValueSpace.GetFSObjectContentsByObjectId(
    NewObject.BoldObjectLocator.BoldObjectID);

  Assert.IsNotNull(oc, 'Object contents should be recorded in undo block');
  Assert.AreEqual(besNotCreated, oc.BoldExistenceState,
    'New object should be recorded with besNotCreated state for proper undo');
end;

procedure TTestBoldUndoHandler.TestDeletedObjectRecordedWithCorrectExistenceState;
var
  NewObject: TSomeClass;
  ObjectId: TBoldObjectId;
  oc: TBoldFreeStandingObjectContents;
begin
  // Create and save an object first
  GenerateObjects(System, 'SomeClass', 1);
  RefreshSystem;

  // Get the first object and delete it
  NewObject := FSomeClassList[0];
  ObjectId := NewObject.BoldObjectLocator.BoldObjectID.Clone;
  try
    UndoHandler.SetCheckPoint('BeforeDelete');
    NewObject.Delete;

    // Get the object contents from the undo block
    oc := UndoHandler.UndoBlocks.CurrentBlock.FSValueSpace.GetFSObjectContentsByObjectId(ObjectId);

    Assert.IsNotNull(oc, 'Deleted object contents should be recorded in undo block');
    Assert.AreEqual(besExisting, oc.BoldExistenceState,
      'Deleted object should be recorded with besExisting state for proper undo');
  finally
    ObjectId.Free;
  end;
end;

// Attribute modification tests

procedure TTestBoldUndoHandler.TestModifyAttributeRecordsOldValue;
var
  Obj: TSomeClass;
  OldValue: string;
  RecordedValue: IBoldValue;
begin
  // Create and save an object
  GenerateObjects(System, 'SomeClass', 1);
  RefreshSystem;

  Obj := FSomeClassList[0];
  OldValue := Obj.aString;

  UndoHandler.SetCheckPoint('BeforeModify');
  Obj.aString := 'ModifiedValue';

  // Verify the old value is recorded in undo block
  Assert.IsTrue(
    UndoHandler.UndoBlocks.CurrentBlock.ValueExists(
      Obj.BoldObjectLocator.BoldObjectID,
      Obj.M_aString.BoldMemberRTInfo.Index,
      RecordedValue),
    'Old value should be recorded in undo block');
end;

procedure TTestBoldUndoHandler.TestUndoRestoresAttributeValue;
var
  Obj: TSomeClass;
  OldValue: string;
  ObjectId: TBoldObjectId;
begin
  // Create and save an object
  GenerateObjects(System, 'SomeClass', 1);
  RefreshSystem;

  Obj := FSomeClassList[0];
  OldValue := Obj.aString;
  ObjectId := Obj.BoldObjectLocator.BoldObjectID.Clone;
  try
    UndoHandler.SetCheckPoint('BeforeModify');
    Obj.aString := 'ModifiedValue';

    Assert.AreEqual('ModifiedValue', Obj.aString, 'Value should be modified');

    // Undo the change
    UndoHandler.UndoLatest;

    // Re-fetch the object (it may have been recreated)
    Obj := System.Locators.ObjectByID[ObjectId] as TSomeClass;

    Assert.AreEqual(OldValue, Obj.aString, 'Undo should restore original value');
  finally
    ObjectId.Free;
  end;
end;

// Undo/Redo cycle tests

procedure TTestBoldUndoHandler.TestUndoObjectCreation;
var
  ParentObj, ChildObj: TSomeClass;
  ParentId, ChildId: TBoldObjectId;
  BlockName: string;
begin
  // Create a parent object and save it
  ParentObj := CreateSomeClass(System, nil, True);
  UpdateDatabase;
  ParentId := ParentObj.BoldObjectLocator.BoldObjectID.Clone;
  try
    // Set checkpoint and create a child object linked to parent
    BlockName := UndoHandler.SetCheckPoint('CreateChild');
    ChildObj := CreateSomeClass(System, nil, True);
    ChildId := ChildObj.BoldObjectLocator.BoldObjectID.Clone;
    try
      ChildObj.parent := ParentObj;

      Assert.AreEqual(1, ParentObj.child.Count, 'Parent should have 1 child');

      // Undo the child creation
      UndoHandler.UndoBlock(BlockName);

      // Re-fetch parent
      ParentObj := System.Locators.ObjectByID[ParentId] as TSomeClass;
      ChildObj := System.Locators.ObjectByID[ChildId] as TSomeClass;

      Assert.IsTrue(ParentObj.BoldExistenceState = besExisting, 'Parent should still exist');
      Assert.IsNull(ChildObj, 'Child should be deleted after undo');
      Assert.AreEqual(0, ParentObj.child.Count, 'Parent should have no children after undo');
    finally
      ChildId.Free;
    end;
  finally
    ParentId.Free;
  end;
end;

procedure TTestBoldUndoHandler.TestRedoObjectCreation;
var
  ParentObj, ChildObj: TSomeClass;
  ParentId, ChildId: TBoldObjectId;
  BlockName: string;
begin
  // Create a parent object and save it
  ParentObj := CreateSomeClass(System, nil, True);
  UpdateDatabase;
  ParentId := ParentObj.BoldObjectLocator.BoldObjectID.Clone;
  try
    // Set checkpoint and create a child object linked to parent
    BlockName := UndoHandler.SetCheckPoint('CreateChild');
    ChildObj := CreateSomeClass(System, nil, True);
    ChildId := ChildObj.BoldObjectLocator.BoldObjectID.Clone;
    try
      ChildObj.parent := ParentObj;

      // Undo
      UndoHandler.UndoBlock(BlockName);

      // Redo
      UndoHandler.RedoBlock(BlockName);

      // Re-fetch objects
      ParentObj := System.Locators.ObjectByID[ParentId] as TSomeClass;
      ChildObj := System.Locators.ObjectByID[ChildId] as TSomeClass;

      Assert.IsTrue(ParentObj.BoldExistenceState = besExisting, 'Parent should exist after redo');
      Assert.IsTrue(ChildObj.BoldExistenceState = besExisting, 'Child should exist after redo');
      Assert.AreEqual(1, ParentObj.child.Count, 'Parent should have 1 child after redo');
    finally
      ChildId.Free;
    end;
  finally
    ParentId.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldUndoHandler);

end.
