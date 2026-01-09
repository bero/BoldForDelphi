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
    FAPersistentClassList: TAPersistentClassList;
    FATransientClassList: TATransientClassList;
    FFSValueSpace: TBoldFreeStandingValueSpace;
    function GetSystem: TBoldSystem;
    function GetUndoHandler: TBoldUndoHandler;
    procedure RefreshSystem;
    procedure UpdateDatabase;
    // Helper methods for verification
    procedure StoreValue(const Member: TBoldMember);
    function GetStoredValueOfMember(const Member: TBoldMember): IBoldValue;
    procedure VerifyState(const Element: TBoldDomainElement; const State: TBoldValuePersistenceState);
    procedure VerifyIsInUndoArea(aBlock: TBoldUndoBlock; Member: TBoldMember; MemberValue: IBoldValue);
    procedure VerifyIsInRedoArea(Member: TBoldMember; Value: TBoldFreeStandingValue);
    function IdCompare(Item1, Item2: TBoldElement): Integer;
    procedure FetchClassSorted(const aSystem: TBoldSystem; const aList: TBoldObjectList; const ObjClass: TBoldObjectClass);
    procedure SetSimpleConfiguration;
    procedure SetTransientConfiguration;
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

    // Tests migrated from maan_Modify

    [Test]
    [Category('Quick')]
    procedure TestModifyPersistentAttributeCurrent;

    [Test]
    [Category('Quick')]
    procedure TestModifyPersistentAttributeModified;

    [Test]
    [Category('Quick')]
    procedure TestModifyTransientAttribute;

    [Test]
    [Category('Quick')]
    procedure TestCreatePersistentObjectRecordsUndoState;

    [Test]
    [Category('Quick')]
    procedure TestCreateTransientObjectRecordsUndoState;

    [Test]
    [Category('Quick')]
    procedure TestDeleteTransientObjectRecordsUndoState;

    [Test]
    [Category('Quick')]
    procedure TestModifyEmbeddedRoleCurrent;

    [Test]
    [Category('Quick')]
    procedure TestModifyEmbeddedRoleTransient;

    [Test]
    [Category('Quick')]
    procedure TestModifyNonEmbeddedRoleInsertCurrent;

    [Test]
    [Category('Quick')]
    procedure TestModifyNonEmbeddedRoleDeleteCurrent;

    [Test]
    [Category('Quick')]
    procedure TestModifyNonEmbeddedRoleInsertTransient;

    [Test]
    [Category('Quick')]
    procedure TestModifyNonEmbeddedRoleDeleteTransient;

    // Tests migrated from maan_Undo

    [Test]
    [Category('Quick')]
    procedure TestUndoTransientAttribute;

    [Test]
    [Category('Quick')]
    procedure TestUndoModifiedAttribute;

    [Test]
    [Category('Quick')]
    procedure TestUndoEmbeddedRoleModified;

    [Test]
    [Category('Quick')]
    procedure TestUndoEmbeddedRoleTransient;

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
  FAPersistentClassList := TAPersistentClassList.Create;
  FATransientClassList := TATransientClassList.Create;
  FFSValueSpace := TBoldFreeStandingValueSpace.Create;
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
  FreeAndNil(FAPersistentClassList);
  FreeAndNil(FATransientClassList);
  FreeAndNil(FFSValueSpace);
end;

function TTestBoldUndoHandler.GetSystem: TBoldSystem;
begin
  Result := dmUndoRedo.BoldSystemHandle1.System;
end;

function TTestBoldUndoHandler.GetUndoHandler: TBoldUndoHandler;
begin
  Result := FUndoHandler;
end;

function TTestBoldUndoHandler.IdCompare(Item1, Item2: TBoldElement): Integer;
var
  i1, i2: Integer;
begin
  i1 := StrToInt(TBoldObject(Item1).BoldObjectLocator.AsString);
  i2 := StrToInt(TBoldObject(Item2).BoldObjectLocator.AsString);
  if i1 = i2 then
    Result := 0
  else if i1 < i2 then
    Result := -1
  else
    Result := 1;
end;

procedure TTestBoldUndoHandler.FetchClassSorted(const aSystem: TBoldSystem;
  const aList: TBoldObjectList; const ObjClass: TBoldObjectClass);
begin
  FetchClass(aSystem, aList, ObjClass);
  aList.Sort(IdCompare);
end;

procedure TTestBoldUndoHandler.RefreshSystem;
begin
  UpdateDatabase;
  dmUndoRedo.BoldSystemHandle1.Active := False;
  FSomeClassList.Clear;
  FAPersistentClassList.Clear;
  FATransientClassList.Clear;
  dmUndoRedo.BoldSystemHandle1.Active := True;
  FUndoHandler := dmUndoRedo.BoldSystemHandle1.System.UndoHandler as TBoldUndoHandler;
  FUndoHandler.Enabled := True;
  FetchClassSorted(System, FSomeClassList, TSomeClass);
end;

procedure TTestBoldUndoHandler.UpdateDatabase;
begin
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
end;

procedure TTestBoldUndoHandler.SetSimpleConfiguration;
begin
  GenerateObjects(System, 'SomeClass', 4);
  UpdateDatabase;
  FetchClassSorted(System, FSomeClassList, TSomeClass);
  FSomeClassList[1].parent := FSomeClassList[0];
  FSomeClassList[3].parent := FSomeClassList[2];
end;

procedure TTestBoldUndoHandler.SetTransientConfiguration;
begin
  RefreshSystem;
  GenerateObjects(System, 'APersistentClass', 2);
  RefreshSystem;
  FetchEnsuredClass(System, FAPersistentClassList, TAPersistentClass);
  CreateATransientClass(System, nil);
  CreateATransientClass(System, nil);
  FetchClassSorted(System, FATransientClassList, TATransientClass);
  FATransientClassList[0].many.Add(FAPersistentClassList[0]);
  FATransientClassList[1].many.Add(FAPersistentClassList[1]);
end;

procedure TTestBoldUndoHandler.StoreValue(const Member: TBoldMember);
var
  oc: TBoldFreeStandingObjectContents;
  MemberId: TBoldMemberId;
begin
  (FFSValueSpace as IBoldValueSpace).EnsureObjectContents(Member.OwningObject.BoldObjectLocator.BoldObjectID);
  oc := FFSValueSpace.GetFSObjectContentsByObjectId(Member.OwningObject.BoldObjectLocator.BoldObjectID);
  oc.ApplyObjectContents(Member.OwningObject.AsIBoldObjectContents[bdepContents], False, False);
  MemberId := TBoldMemberID.Create(Member.BoldMemberRTInfo.Index);
  try
    oc.EnsureMember(MemberId, Member.AsIBoldValue[bdepContents].ContentName);
    oc.ValueByIndex[MemberId.MemberIndex].AssignContent(Member.AsIBoldValue[bdepContents]);
  finally
    FreeAndNil(MemberId);
  end;
end;

function TTestBoldUndoHandler.GetStoredValueOfMember(const Member: TBoldMember): IBoldValue;
var
  oc: TBoldFreeStandingObjectContents;
begin
  oc := FFSValueSpace.GetFSObjectContentsByObjectId(Member.OwningObject.BoldObjectLocator.BoldObjectID);
  Result := oc.ValueByIndex[Member.BoldMemberRTInfo.Index];
end;

procedure TTestBoldUndoHandler.VerifyState(const Element: TBoldDomainElement;
  const State: TBoldValuePersistenceState);
var
  CurState: TBoldValuePersistenceState;
begin
  if Element is TBoldObject then
    CurState := (Element as TBoldObject).BoldPersistenceState
  else if Element is TBoldMember then
    CurState := (Element as TBoldMember).BoldPersistenceState
  else
    raise EBold.Create('VerifyState: unsupported element type');
  Assert.AreEqual(State, CurState,
    Format('%s state should be %d but was %d', [Element.DisplayName, Ord(State), Ord(CurState)]));
end;

procedure TTestBoldUndoHandler.VerifyIsInUndoArea(aBlock: TBoldUndoBlock;
  Member: TBoldMember; MemberValue: IBoldValue);
var
  OldValue: IBoldValue;
  ValueFound: Boolean;
begin
  ValueFound := aBlock.ValueExists(
    Member.OwningObject.BoldObjectLocator.BoldObjectID,
    Member.BoldMemberRTInfo.Index,
    OldValue);
  Assert.IsTrue(ValueFound, Format('%s should be in undo area', [Member.DisplayName]));
  Assert.IsNotNull(OldValue, Format('%s old value should not be nil', [Member.DisplayName]));
end;

procedure TTestBoldUndoHandler.VerifyIsInRedoArea(Member: TBoldMember;
  Value: TBoldFreeStandingValue);
var
  ValueInBlock: IBoldValue;
  Found: Boolean;
begin
  ValueInBlock := nil;
  Found := UndoHandler.RedoBlocks.CurrentBlock.ValueExists(
    Member.OwningObject.BoldObjectLocator.BoldObjectID,
    Member.BoldMemberRTInfo.Index,
    ValueInBlock);
  Assert.IsTrue(Found, Format('%s should be in redo area', [Member.DisplayName]));
  Assert.IsNotNull(ValueInBlock, Format('%s redo value should not be nil', [Member.DisplayName]));
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

// Tests migrated from maan_Modify

procedure TTestBoldUndoHandler.TestCreatePersistentObjectRecordsUndoState;
var
  NewObject: TSomeClass;
  oc: TBoldFreeStandingObjectContents;
begin
  RefreshSystem;
  NewObject := CreateSomeClass(System, nil, True);
  oc := UndoHandler.UndoBlocks.CurrentBlock.FSValueSpace.GetFSObjectContentsByObjectId(
    NewObject.BoldObjectLocator.BoldObjectID);
  Assert.IsNotNull(oc, 'oc should be assigned after CreateSomeClass');
  Assert.AreEqual(besNotCreated, oc.BoldExistenceState,
    'oc.BoldExistenceState should be besNotCreated');
end;

procedure TTestBoldUndoHandler.TestCreateTransientObjectRecordsUndoState;
var
  NewObject: TATransientClass;
  oc: TBoldFreeStandingObjectContents;
begin
  RefreshSystem;
  NewObject := CreateATransientClass(System, nil);
  Assert.AreEqual(besExisting, NewObject.BoldExistenceState,
    'NewObject.BoldExistenceState should be besExisting');
  oc := UndoHandler.UndoBlocks.CurrentBlock.FSValueSpace.GetFSObjectContentsByObjectId(
    NewObject.BoldObjectLocator.BoldObjectID);
  Assert.IsNotNull(oc, 'oc should be assigned for transient object');
  Assert.AreEqual(besNotCreated, oc.BoldExistenceState,
    'oc.BoldExistenceState should be besNotCreated for transient');
end;

procedure TTestBoldUndoHandler.TestDeleteTransientObjectRecordsUndoState;
var
  NewObject: TATransientClass;
  oc: TBoldFreeStandingObjectContents;
  oid: TBoldObjectId;
begin
  RefreshSystem;
  NewObject := CreateATransientClass(System, nil);
  Assert.AreEqual(besExisting, NewObject.BoldExistenceState,
    'NewObject.BoldExistenceState should be besExisting before Delete');
  oid := NewObject.BoldObjectLocator.BoldObjectID.Clone;
  try
    UndoHandler.SetCheckPoint('Delete NewObject');
    NewObject.Delete;
    oc := UndoHandler.UndoBlocks.CurrentBlock.FSValueSpace.GetFSObjectContentsByObjectId(oid);
    Assert.IsNotNull(oc, 'oc should be assigned after Delete transient');
    Assert.AreEqual(besExisting, oc.BoldExistenceState,
      'oc.BoldExistenceState should be besExisting after Delete transient');
  finally
    oid.Free;
  end;
end;

procedure TTestBoldUndoHandler.TestModifyPersistentAttributeCurrent;
var
  NewObject: TSomeClass;
begin
  GenerateObjects(System, 'SomeClass', 1);
  RefreshSystem;
  NewObject := FSomeClassList[0];
  VerifyState(NewObject.M_aString, bvpsCurrent);
  StoreValue(NewObject.M_aString);
  NewObject.aString := NewObject.aString + '123';
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, NewObject.M_aString,
    GetStoredValueOfMember(NewObject.M_aString));
  VerifyState(NewObject.M_aString, bvpsModified);
end;

procedure TTestBoldUndoHandler.TestModifyPersistentAttributeModified;
var
  NewObject: TSomeClass;
begin
  GenerateObjects(System, 'SomeClass', 1);
  RefreshSystem;
  NewObject := FSomeClassList[0];
  NewObject.aString := NewObject.aString + '123';
  VerifyState(NewObject.M_aString, bvpsModified);
  StoreValue(NewObject.M_aString);
  NewObject.aString := NewObject.aString + '456';
  VerifyState(NewObject.M_aString, bvpsModified);
end;

procedure TTestBoldUndoHandler.TestModifyTransientAttribute;
var
  NewObject: TATransientClass;
begin
  RefreshSystem;
  NewObject := CreateATransientClass(System, nil);
  VerifyState(NewObject.M_aString, bvpsTransient);
  StoreValue(NewObject.M_aString);
  UndoHandler.SetCheckPoint('Block1');
  NewObject.aString := NewObject.aString + '123';
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, NewObject.M_aString,
    GetStoredValueOfMember(NewObject.M_aString));
  VerifyState(NewObject.M_aString, bvpsTransient);
end;

procedure TTestBoldUndoHandler.TestModifyEmbeddedRoleCurrent;
var
  ObjA, ObjB, ObjA2: TSomeClass;
begin
  SetSimpleConfiguration;
  RefreshSystem;

  ObjA := FSomeClassList[0];
  ObjB := FSomeClassList[1];
  ObjA2 := FSomeClassList[2];
  ObjB.M_parent.EnsureContentsCurrent;
  VerifyState(ObjB.M_parent, bvpsCurrent);

  // ObjA.child Current
  ObjA.child.EnsureContentsCurrent;
  ObjA2.child.EnsureContentsCurrent;
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState(ObjA2.M_child, bvpsCurrent);
  StoreValue(ObjB.M_parent);
  Assert.IsTrue(ObjB.parent = ObjA, 'ObjB.parent should be ObjA');
  ObjB.parent := ObjA2; // modify
  VerifyState(ObjB.M_parent, bvpsModified);
  Assert.IsTrue(ObjB.parent = ObjA2, 'ObjB.parent should now be ObjA2');
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB.M_parent,
    GetStoredValueOfMember(ObjB.M_parent));
  VerifyState(ObjA.M_child, bvpsCurrent);
  Assert.IsFalse(ObjA.child.Includes(ObjB), 'ObjA.child should not include ObjB');
  VerifyState(ObjA2.M_child, bvpsCurrent);
  Assert.IsTrue(ObjA2.child.Includes(ObjB), 'ObjA2.child should include ObjB');
end;

procedure TTestBoldUndoHandler.TestModifyEmbeddedRoleTransient;
var
  ObjA1, ObjA2: TATransientClass;
  ObjB1, ObjB2: TAPersistentClass;
begin
  // EmbeddedRole.child Transient
  GenerateObjects(System, 'APersistentClass', 2);
  RefreshSystem;
  FetchEnsuredClass(System, FAPersistentClassList, TAPersistentClass);
  ObjB1 := FAPersistentClassList[0];
  ObjB2 := FAPersistentClassList[1];
  ObjA1 := CreateATransientClass(System, nil);
  ObjA2 := CreateATransientClass(System, nil);
  ObjA1.many.Add(ObjB1);
  ObjA2.many.Add(ObjB2);
  VerifyState(ObjB1.M_one, bvpsTransient);
  VerifyState(ObjA1.M_many, bvpsTransient);
  VerifyState(ObjA2.M_many, bvpsTransient);
  UndoHandler.SetCheckPoint('ModifyEmbeddedTransient');
  StoreValue(ObjB1.M_one);
  Assert.IsTrue(ObjB1.one = ObjA1, 'ObjB1.one should be ObjA1');
  ObjB1.one := ObjA2; // modify
  VerifyState(ObjA1.M_many, bvpsTransient);
  VerifyState(ObjA2.M_many, bvpsTransient);
  VerifyState(ObjB1.M_one, bvpsTransient);
  Assert.IsTrue(ObjB2.one = ObjA2, 'ObjB2.one should be ObjA2');
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB1.M_one,
    GetStoredValueOfMember(ObjB1.M_one));
  Assert.IsFalse(ObjA1.many.Includes(ObjB1), 'ObjA1.many should not include ObjB1');
  Assert.IsTrue(ObjA2.many.Includes(ObjB1), 'ObjA2.many should include ObjB1');
end;

procedure TTestBoldUndoHandler.TestModifyNonEmbeddedRoleInsertCurrent;
var
  ObjA, ObjB: TSomeClass;
  ObjBLocator: TBoldObjectLocator;
begin
  SetSimpleConfiguration;
  // Change setup: ObjB.parent points to ObjA2 (index 3), not ObjA (index 0)
  FSomeClassList[1].parent := FSomeClassList[3];
  FSomeClassList[3].parent := FSomeClassList[2];
  RefreshSystem;

  ObjA := FSomeClassList[0];
  ObjBLocator := FSomeClassList.Locators[1];
  ObjA.child.EnsureContentsCurrent;
  VerifyState(ObjA.M_child, bvpsCurrent);

  // ObjB.parent current - insert ObjB into ObjA.child
  ObjB := ObjBLocator.EnsuredBoldObject as TSomeClass;
  ObjB.parent; // ensure current
  VerifyState(ObjB.M_parent, bvpsCurrent);
  StoreValue(ObjB.M_parent);
  ObjA.child.Add(ObjB); // modify
  VerifyState(ObjB.M_parent, bvpsModified);
  Assert.IsTrue((ObjB.M_parent as TBoldObjectReference).Locator = ObjA.BoldObjectLocator,
    'ObjB.parent should point to ObjA');
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB.M_parent,
    GetStoredValueOfMember(ObjB.M_parent));
  VerifyState(ObjA.M_child, bvpsCurrent);
  Assert.IsTrue(ObjA.child.Includes(ObjB), 'ObjA.child should include ObjB');
end;

procedure TTestBoldUndoHandler.TestModifyNonEmbeddedRoleDeleteCurrent;
var
  ObjA, ObjB: TSomeClass;
  ObjBLocator: TBoldObjectLocator;
begin
  SetSimpleConfiguration;
  RefreshSystem;

  ObjA := FSomeClassList[0];
  ObjBLocator := FSomeClassList.Locators[1];
  ObjA.child.EnsureContentsCurrent;
  VerifyState(ObjA.M_child, bvpsCurrent);

  // ObjB.parent current
  ObjB := ObjBLocator.EnsuredBoldObject as TSomeClass;
  ObjB.parent; // ensure current
  VerifyState(ObjB.M_parent, bvpsCurrent);
  StoreValue(ObjB.M_parent);
  ObjA.child.Remove(ObjB); // modify-delete
  Assert.IsTrue(ObjB.parent = nil, 'ObjB.parent should be nil after remove');
  VerifyState(ObjB.M_parent, bvpsModified);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB.M_parent,
    GetStoredValueOfMember(ObjB.M_parent));
  VerifyState(ObjA.M_child, bvpsCurrent);
  Assert.IsFalse(ObjA.child.Includes(ObjB), 'ObjA.child should not include ObjB');
end;

procedure TTestBoldUndoHandler.TestModifyNonEmbeddedRoleInsertTransient;
var
  ObjA1, ObjA2: TATransientClass;
  ObjB1, ObjB2: TAPersistentClass;
begin
  SetTransientConfiguration;
  ObjB1 := FAPersistentClassList[0];
  ObjB2 := FAPersistentClassList[1];
  ObjA1 := FATransientClassList[0];
  ObjA2 := FATransientClassList[1];
  VerifyState(ObjB1.M_one, bvpsTransient);
  VerifyState(ObjA1.M_many, bvpsTransient);
  VerifyState(ObjA2.M_many, bvpsTransient);
  StoreValue(ObjB2.M_one);
  UndoHandler.SetCheckPoint('Check1');
  Assert.IsTrue(ObjB2.one = ObjA2, 'ObjB2.one should be ObjA2');
  ObjA1.many.Add(ObjB2); // modify
  VerifyState(ObjA1.M_many, bvpsTransient);
  VerifyState(ObjA2.M_many, bvpsTransient);
  VerifyState(ObjB1.M_one, bvpsTransient);
  VerifyState(ObjB2.M_one, bvpsTransient);
  Assert.IsTrue(ObjB2.one = ObjA1, 'ObjB2.one should now be ObjA1');
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB2.M_one,
    GetStoredValueOfMember(ObjB2.M_one));
  Assert.IsTrue(ObjA1.many.Includes(ObjB2), 'ObjA1.many should include ObjB2');
  Assert.IsFalse(ObjA2.many.Includes(ObjB2), 'ObjA2.many should not include ObjB2');
end;

procedure TTestBoldUndoHandler.TestModifyNonEmbeddedRoleDeleteTransient;
var
  ObjA1: TATransientClass;
  ObjB1: TAPersistentClass;
begin
  SetTransientConfiguration;
  ObjA1 := FATransientClassList[0];
  ObjB1 := FAPersistentClassList[0];
  VerifyState(ObjB1.M_one, bvpsTransient);
  VerifyState(ObjA1.M_many, bvpsTransient);
  StoreValue(ObjB1.M_one);
  UndoHandler.SetCheckPoint('Check1');
  ObjA1.many.Remove(ObjB1); // modify
  VerifyState(ObjA1.M_many, bvpsTransient);
  VerifyState(ObjB1.M_one, bvpsTransient);
  Assert.IsTrue(ObjB1.one = nil, 'ObjB1.one should be nil after remove');
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB1.M_one,
    GetStoredValueOfMember(ObjB1.M_one));
  Assert.IsFalse(ObjA1.many.Includes(ObjB1), 'ObjA1.many should not include ObjB1');
end;

// Tests migrated from maan_Undo

procedure TTestBoldUndoHandler.TestUndoTransientAttribute;
var
  TransientObject: TATransientClass;
  TransientObjectId: TBoldObjectId;
begin
  SetSimpleConfiguration;
  RefreshSystem;

  // Transient attribute undo
  TransientObject := CreateATransientClass(System, nil);
  TransientObjectId := TransientObject.BoldObjectLocator.BoldObjectId.Clone;
  try
    UndoHandler.SetCheckPoint('UndoAttribute');
    StoreValue(TransientObject.M_aString);
    TransientObject.aString := TransientObject.aString + '123';
    VerifyState(TransientObject.M_aString, bvpsTransient);

    UndoHandler.UndoLatest;
    System.AssertLinkIntegrity;

    TransientObject := System.Locators.ObjectByID[TransientObjectId] as TATransientClass;
    Assert.IsTrue(TransientObject.ValuesAreEqual(
      TransientObject.M_aString.AsIBoldValue[bdepContents],
      GetStoredValueOfMember(TransientObject.M_aString), 'aString'),
      'Value should be restored after undo');
    VerifyState(TransientObject.M_aString, bvpsTransient);
  finally
    TransientObjectId.Free;
  end;
end;

procedure TTestBoldUndoHandler.TestUndoModifiedAttribute;
var
  SomeObject: TSomeClass;
  SomeObjectId: TBoldObjectId;
begin
  SetSimpleConfiguration;
  RefreshSystem;

  SomeObject := FSomeClassList[0];
  SomeObjectId := SomeObject.BoldObjectLocator.BoldObjectId.Clone;
  try
    StoreValue(SomeObject.M_aString);
    VerifyState(SomeObject.M_aString, bvpsCurrent);
    SomeObject.aString := SomeObject.aString + '123';
    VerifyState(SomeObject.M_aString, bvpsModified);

    UndoHandler.UndoLatest;
    System.AssertLinkIntegrity;

    SomeObject := System.Locators.ObjectById[SomeObjectId] as TSomeClass;
    VerifyState(SomeObject.M_aString, bvpsCurrent);
    Assert.IsTrue(SomeObject.ValuesAreEqual(
      SomeObject.M_aString.AsIBoldValue[bdepContents],
      GetStoredValueOfMember(SomeObject.M_aString), 'aString'),
      'Value should be restored after undo');
  finally
    SomeObjectId.Free;
  end;
end;

procedure TTestBoldUndoHandler.TestUndoEmbeddedRoleModified;
var
  ObjA1, ObjA2, ObjB1, ObjB2: TSomeClass;
  ObjA1Id, ObjA2Id, ObjB1Id, ObjB2Id: TBoldObjectId;
  FSValue: TBFSObjectIdRef;

  procedure Prepare;
  begin
    RefreshSystem;
    FSomeClassList[1].parent := FSomeClassList[0];
    FSomeClassList[3].parent := FSomeClassList[2];
    RefreshSystem;
    ObjA1 := FSomeClassList[0];
    ObjA1Id := ObjA1.BoldObjectLocator.BoldObjectId.Clone;
    ObjB1 := FSomeClassList[1];
    ObjB1Id := ObjB1.BoldObjectLocator.BoldObjectId.Clone;
    ObjA2 := FSomeClassList[2];
    ObjA2Id := ObjA2.BoldObjectLocator.BoldObjectId.Clone;
    ObjB2 := FSomeClassList[3];
    ObjB2Id := ObjB2.BoldObjectLocator.BoldObjectId.Clone;
  end;

  procedure SetObjectsFromIds;
  begin
    ObjA1 := System.Locators.ObjectById[ObjA1Id] as TSomeClass;
    ObjB1 := System.Locators.ObjectById[ObjB1Id] as TSomeClass;
    ObjA2 := System.Locators.ObjectById[ObjA2Id] as TSomeClass;
    ObjB2 := System.Locators.ObjectById[ObjB2Id] as TSomeClass;
  end;

begin
  SetSimpleConfiguration;
  Prepare;

  // NonEmbeddedRole.parent invalid, ObjA1 invalid, ObjA2 invalid
  FSValue := TBFSObjectIdRef.Create;
  try
    StoreValue(ObjB1.M_parent);
    VerifyState(ObjB1.M_parent, bvpsCurrent);
    ObjB1.parent := ObjA2; // modify
    VerifyState(ObjA1.M_child, bvpsInvalid);
    VerifyState(ObjA2.M_child, bvpsInvalid);
    VerifyState(ObjB1.M_parent, bvpsModified);
    FSValue.AssignContent(ObjB1.M_parent.AsIBoldValue[bdepContents]);

    UndoHandler.UndoLatest;
    System.AssertLinkIntegrity;
    SetObjectsFromIds;

    VerifyState(ObjB1.M_parent, bvpsCurrent);
    Assert.IsTrue(ObjB1.ValuesAreEqual(
      ObjB1.M_parent.AsIBoldValue[bdepContents],
      GetStoredValueOfMember(ObjB1.M_parent), 'parent'),
      'Parent should be restored after undo');
    VerifyIsInRedoArea(ObjB1.M_parent, FSValue);
    VerifyState(ObjA2.M_child, bvpsInvalid);
    VerifyState(ObjA1.M_child, bvpsInvalid);
  finally
    FreeAndNil(FSValue);
    ObjA1Id.Free;
    ObjA2Id.Free;
    ObjB1Id.Free;
    ObjB2Id.Free;
  end;
end;

procedure TTestBoldUndoHandler.TestUndoEmbeddedRoleTransient;
var
  ObjA1, ObjA2: TATransientClass;
  ObjB2: TAPersistentClass;
  ObjA1Id, ObjA2Id, ObjB2Id: TBoldObjectId;
  FSValueBeforeUndo: TBFSObjectIdRef;
  BlockName: string;

  procedure SetObjectsFromIds;
  begin
    ObjA1 := System.Locators.ObjectById[ObjA1Id] as TATransientClass;
    ObjA2 := System.Locators.ObjectById[ObjA2Id] as TATransientClass;
    ObjB2 := System.Locators.ObjectById[ObjB2Id] as TAPersistentClass;
  end;

begin
  SetTransientConfiguration;
  ObjB2 := FAPersistentClassList[1];
  ObjB2Id := ObjB2.BoldObjectLocator.BoldObjectId.Clone;
  ObjA1 := FATransientClassList[0];
  ObjA1Id := ObjA1.BoldObjectLocator.BoldObjectId.Clone;
  ObjA2 := FATransientClassList[1];
  ObjA2Id := ObjA2.BoldObjectLocator.BoldObjectId.Clone;
  FSValueBeforeUndo := TBFSObjectIdRef.Create;
  try
    BlockName := UndoHandler.SetCheckPoint;
    StoreValue(ObjB2.M_one);
    ObjB2.one := ObjA1; // modify
    VerifyState(ObjB2.M_one, bvpsTransient);
    VerifyState(ObjA1.M_many, bvpsTransient);
    VerifyState(ObjA2.M_many, bvpsTransient);
    FSValueBeforeUndo.AssignContent(ObjB2.M_one.AsIBoldValue[bdepContents]);

    UndoHandler.UndoLatest;
    System.AssertLinkIntegrity;
    SetObjectsFromIds;

    VerifyState(ObjB2.M_one, bvpsTransient);
    Assert.IsTrue(ObjB2.ValuesAreEqual(
      ObjB2.M_one.AsIBoldValue[bdepContents],
      GetStoredValueOfMember(ObjB2.M_one), 'one'),
      'One should be restored after undo');
    VerifyIsInRedoArea(ObjB2.M_one, FSValueBeforeUndo);
    Assert.IsFalse(ObjA1.many.Includes(ObjB2), 'ObjA1.many should not include ObjB2 after undo');
    Assert.IsTrue(ObjA2.many.Includes(ObjB2), 'ObjA2.many should include ObjB2 after undo');
  finally
    FreeAndNil(FSValueBeforeUndo);
    ObjA1Id.Free;
    ObjA2Id.Free;
    ObjB2Id.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldUndoHandler);

end.
