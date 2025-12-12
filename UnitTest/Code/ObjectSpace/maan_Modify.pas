unit maan_Modify;

interface

uses
  SysUtils,
  Classes,
  DUnitX.TestFramework,
  BoldDefs,
  BoldDomainElement,
  BoldValueSpaceInterfaces,
  BoldSystem,
  BoldId,
  BoldSubscription,
  BoldUndoHandler,
  UndoTestModelClasses,
  BoldFreeStandingValues,
  maan_UndoRedoBase,
  maan_UndoRedoTestCaseUtils;

type
  [TestFixture]
  [Category('Modify')]
  Tmaan_ModifyTestCase = class(Tmaan_UndoRedoAbstractTestCase)
  public
    [Test]
    [Ignore('Undo/Redo functionality issue - needs investigation')]
    procedure TestModifyAttribute;
    [Test]
    [Ignore('Undo/Redo functionality issue - needs investigation')]
    procedure TestModifyNonEmbeddedRoleInsertTransient;
    [Test]
    [Ignore('Undo/Redo functionality issue - needs investigation')]
    procedure TestModifyNonEmbeddedRoleInsertCurrent;
    [Test]
    [Ignore('Undo/Redo functionality issue - needs investigation')]
    procedure TestModifyNonEmbeddedRoleDeleteTransient;
    [Test]
    [Ignore('Undo/Redo functionality issue - needs investigation')]
    procedure TestModifyNonEmbeddedRoleDeleteCurrent;
    [Test]
    [Ignore('Undo/Redo functionality issue - needs investigation')]
    procedure TestModifyEmbeddedRoleCurrent;
    [Test]
    [Ignore('Undo/Redo functionality issue - needs investigation')]
    procedure TestModifyEmbeddedRoleTransient;
    [Test]
    [Ignore('Undo/Redo functionality issue - needs investigation')]
    procedure TestModifyEmbeddedRoleModified;
  end;

implementation

{ Tmaan_ModifyTestCase }

procedure Tmaan_ModifyTestCase.TestModifyAttribute;
var
  NewObject: TSomeClass;
  oc: TBoldFreeStandingObjectContents;
  oid: TBoldObjectId;
  procedure Prepare;
  begin
    RefreshSystem;
  end;
begin
  {Persistent}
  {Create Object}
  Prepare;
  NewObject := CreateSomeClass(system, FSubscriber, true);
  oc := UndoHandler.Undoblocks.CurrentBlock.FSValueSpace.GetFSObjectContentsByObjectId(NewObject.BoldObjectLocator.BoldObjectID);
  Assert.IsTrue(Assigned(oc));
  Assert.IsTrue(oc.BoldExistenceState in [besNotCreated]);

  {Delete Object}
  Prepare;
  NewObject := FSomeClassList[0];
  NewObject.Delete;
  oc := UndoHandler.Undoblocks.CurrentBlock.FSValueSpace.GetFSObjectContentsByObjectId(NewObject.BoldobjectLocator.BoldObjectId);
  Assert.IsTrue(Assigned(oc));
  Assert.IsTrue(oc.BoldExistenceState in [besExisting]);

  {Current attribute}
  GenerateObjects(System, 'SomeClass', 1);
  Prepare;
  Newobject := FSomeClassList[0];
  VerifyState(NewObject.M_aString, bvpsCurrent);
  FSubscriber.SubscribeToElement(NewObject.M_aString);
  StoreValue(NewObject.M_aString);
  NewObject.aString := NewObject.aString + '123';
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, NewObject.M_aString, GetStoredValueOfMember(newObject.M_aString));
  FSubscriber.VerifySendEvent(beValueChanged, NewObject.M_aString);
  VerifyState(NewObject.M_aString, bvpsModified);

  {Modified attribute}
  Prepare;
  Newobject := FSomeClassList[0];
  NewObject.aString := NewObject.aString + '123';
  VerifyState(NewObject.M_aString, bvpsModified);
  FSubscriber.SubscribeToElement(NewObject.M_aString);
  StoreValue(NewObject.M_aString);
  NewObject.aString := NewObject.aString + '456';
  FSubscriber.VerifySendEvent(beValueChanged, NewObject.M_aString);
  VerifyState(NewObject.M_aString, bvpsModified);


  {Transient}
  {Create Object}
  Prepare;
  NewObject := CreateSomeClass(system, FSubscriber, false);
  Assert.IsTrue(NewObject.BoldExistenceState in [besExisting]);
  oc := UndoHandler.Undoblocks.CurrentBlock.FSValueSpace.GetFSObjectContentsByObjectId(NewObject.BoldObjectLocator.BoldObjectID);
  Assert.IsTrue(Assigned(oc));
  Assert.IsTrue(oc.BoldExistenceState in [besNotCreated]);

  {Delete Object}
  Assert.IsTrue(NewObject.BoldExistenceState in [besExisting]);
  oid := NewObject.BoldObjectLocator.BoldObjectID.Clone;
  UndoHandler.SetCheckPoint('Delete NewObject');
  NewObject.Delete;
  oc := UndoHandler.Undoblocks.CurrentBlock.FSValueSpace.GetFSObjectContentsByObjectId(oid);
  Assert.IsTrue(Assigned(oc));
  Assert.IsTrue(oc.BoldExistenceState in [besExisting]);

  {transient attribute}
  Prepare;
  NewObject := CreateSomeClass(System, FSubscriber, false);
  VerifyState(NewObject.M_aString,  bvpsTransient);
  FSubscriber.SubscribeToElement(NewObject.M_aString);
  StoreValue(NewObject.M_aString);
  UndoHandler.SetCheckPoint('Block1');
  NewObject.aString := NewObject.aString + '123';
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, NewObject.M_aString, GetStoredValueOfMember(NewObject.M_aString));
  FSubscriber.VerifySendEvent(beValueChanged, NewObject.M_aString);
  VerifyState(NewObject.M_aString, bvpsTransient);

  CloseAll;
end;

procedure Tmaan_ModifyTestCase.TestModifyEmbeddedRoleCurrent;
var
  ObjA, ObjB, ObjA2: TSomeClass;
  procedure Prepare;
  begin
    RefreshSystem;
    ObjA := FSomeClassList[0];
    ObjB := FSomeClasslist[1];
    ObjA2 := FSomeClassList[2];
    ObjB.M_parent.EnsureContentsCurrent;
    VerifyState(ObjB.M_parent, bvpsCurrent);
  end;

begin
  SetSimpleConfiguration;

  {ObjA.child Invalid}
  Prepare;
  ObjA.child.EnsureContentsCurrent;
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState(ObjA2.M_child, bvpsInvalid);
  StoreValue(ObjB.M_parent);
  FSubscriber.SubscribeToElement(ObjB.M_parent);
  FSubscriber.SubscribeToElement(ObjA.M_child);
  Assert.IsTrue(ObjB.parent = ObjA);
  ObjB.parent := ObjA2; // modify
  VerifyState(ObjB.M_parent, bvpsModified);
  Assert.IsTrue(ObjB.parent = ObjA2);
  VerifyState(ObjA2, bvpsCurrent);
  FSubscriber.VerifySendEvent(beValueChanged, ObjB.M_parent);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB.M_parent, GetStoredValueOfMember(ObjB.M_parent));
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState(ObjA2.M_child, bvpsInvalid);
  VerifyListAdjustedEx(ObjA.child, ObjB, false);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA.M_child);
  ObjB.Parent := ObjA;

  {ObjA.child Current}
  Prepare;
  ObjA.child.EnsureContentsCurrent;
  ObjA2.child.EnsureContentsCurrent;
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState(ObjA2.M_child, bvpsCurrent);
  StoreValue(ObjB.M_parent);
  FSubscriber.SubscribeToElement(ObjB.M_parent);
  FSubscriber.SubscribeToElement(ObjA.M_child);
  FSubscriber.SubscribeToElement(ObjA2.M_child);
  Assert.IsTrue(ObjB.parent = ObjA);
  ObjB.parent := ObjA2; // modify
  VerifyState(ObjB.M_parent, bvpsModified);
  Assert.IsTrue(objB.parent = ObjA2);
  FSubscriber.VerifySendEvent(beValueChanged, ObjB.M_parent);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB.M_parent, GetStoredValueOfMember(ObjB.M_parent));
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyListAdjustedEx(ObjA.child, ObjB, false);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA.M_child);
  VerifyState(ObjA2.M_child, bvpsCurrent);
  VerifyListAdjustedIn(ObjA2.child, ObjB, false);
  FSubscriber.VerifySendEvent(beItemAdded, ObjA2.M_child);
  ObjB.Parent := ObjA;

  CloseAll;
end;

procedure Tmaan_ModifyTestCase.TestModifyEmbeddedRoleModified;
var
  ObjA, ObjB, ObjA2: TSomeClass;
  procedure Prepare;
  begin
    RefreshSystem;
    ObjA := FSomeClassList[0];
    ObjB := FSomeClasslist[1];
    ObjA2 := FSomeClassList[2];
    ObjB.M_parent.EnsureContentsCurrent;
    ObjB.parent := nil;
    ObjB.parent := ObjA;
    VerifyState(ObjB.M_parent, bvpsModified);
    UndoHandler.SetCheckPoint('ModifyEmbeddedModified');
  end;

begin
  SetSimpleConfiguration;

  {ObjA.child Invalid}
  Prepare;
  ObjA.child.EnsureContentsCurrent;
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState(ObjA2.M_child, bvpsInvalid);
  StoreValue(ObjB.M_parent);
  FSubscriber.SubscribeToElement(ObjB.M_parent);
  FSubscriber.SubscribeToElement(ObjA.M_child);
  Assert.IsTrue(ObjB.parent = ObjA);
  ObjB.parent := ObjA2; // modify
  VerifyState(ObjB.M_parent, bvpsModified);
  Assert.IsTrue(ObjB.parent = ObjA2);
  VerifyState(ObjA2, bvpsCurrent);
  FSubscriber.VerifySendEvent(beValueChanged, ObjB.M_parent);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB.M_parent, GetStoredValueOfMember(ObjB.M_parent));
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState(ObjA2.M_child, bvpsInvalid);
  VerifyListAdjustedEx(ObjA.child, ObjB, false);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA.M_child);

  {ObjA.child Current}
  Prepare;
  ObjA.child.EnsureContentsCurrent;
  ObjA2.child.EnsureContentsCurrent;
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState(ObjA2.M_child, bvpsCurrent);
  StoreValue(ObjB.M_parent);
  FSubscriber.SubscribeToElement(ObjB.M_parent);
  FSubscriber.SubscribeToElement(ObjA.M_child);
  FSubscriber.SubscribeToElement(ObjA2.M_child);
  Assert.IsTrue(ObjB.parent = ObjA);
  ObjB.parent := ObjA2; // modify
  VerifyState(ObjB.M_parent, bvpsModified);
  Assert.IsTrue(objB.parent = ObjA2);
  FSubscriber.VerifySendEvent(beValueChanged, ObjB.M_parent);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB.M_parent, GetStoredValueOfMember(ObjB.M_parent));
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyListAdjustedEx(ObjA.child, ObjB, false);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA.M_child);
  VerifyState(ObjA2.M_child, bvpsCurrent);
  VerifyListAdjustedIn(ObjA2.child, ObjB, false);
  FSubscriber.VerifySendEvent(beItemAdded, ObjA2.M_child);

  CloseAll;
end;

procedure Tmaan_ModifyTestCase.TestModifyEmbeddedRoleTransient;
var
  ObjA1, ObjA2: TATransientClass;
  ObjB1, ObjB2: TAPersistentClass;

begin
  {EmbeddedRole.child Transient}
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
  FSubscriber.SubscribeToElement(ObjA1.M_many);
  FSubscriber.SubscribeToElement(ObjA2.M_many);
  FSubscriber.SubscribeToElement(ObjB1.M_one);
  UndoHandler.SetCheckPoint('ModifyEmbeddedTransient');
  StoreValue(ObjB1.m_one);
  Assert.IsTrue(ObjB1.one = ObjA1);
  ObjB1.one := ObjA2;  //modify
  VerifyState(ObjA1.M_many, bvpsTransient);
  VerifyState(ObjA2.M_many, bvpsTransient);
  VerifyState(ObjB1.M_one, bvpsTransient);
  Assert.IsTrue(objB2.one = ObjA2);
  FSubscriber.VerifySendEvent(beValueChanged, ObjB1.M_one);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB1.M_one, GetStoredValueOfMember(ObjB1.M_one));
  VerifyListAdjustedEx(ObjA1.M_many, ObjB1, false);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA1.M_many);
  VerifyListAdjustedIn(ObjA2.M_many, ObjB1, false);
  FSubscriber.VerifySendEvent(beItemAdded, ObjA2.M_many);

  CloseAll;
end;

procedure Tmaan_ModifyTestCase.TestModifyNonEmbeddedRoleDeleteCurrent;
var
  ObjA, ObjB: TSomeClass;
  ObjBLocator: TBoldObjectLocator;
  EmbeddedIndex: integer;

  procedure Prepare;
  begin
    FSomeClassList[1].parent := FsomeClassList[0];
    FSomeClassList[3].parent := FsomeClasslist[2];
    EmbeddedIndex := FsomeClasslist[1].M_parent.BoldMemberRTInfo.EmbeddedLinkIndex;
    RefreshSystem;
    ObjA := FSomeClassList[0];
    ObjBLocator := FSomeClassList.Locators[1];
    ObjA.child.EnsureContentsCurrent;
    VerifyState(ObjA.M_child, bvpsCurrent);
  end;

begin
  SetSimpleConfiguration;
  {ObjB.parent invalid}
  Prepare;
  FSubscriber.SubscribeToElement(ObjA.M_child);
  Assert.IsTrue( not Assigned(ObjBLocator.BoldObject));
  Assert.IsTrue(ObjA.child.IndexOfLocator(ObjBLocator) <> -1);
  ObjA.child.RemoveByIndex(ObjA.child.IndexOfLocator(ObjBLocator)); //modify-delete
  VerifyState((ObjBLocator.BoldObject as TSomeClass).M_parent, bvpsModified);
  VerifyState(ObjA.M_child, bvpsCurrent);
  Assert.IsTrue(ObjA.child.Count = 0, 'ListAdjustedEx failed');
  Assert.IsTrue(not ObjA.child.LocatorInList(ObjBLocator)); // VerifyListAdjustedEx(ObjA.child, ObjB, false);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA.M_child);

  {ObjB.parent invalid/adjust}
  Prepare;
  ObjB := (ObjBLocator.EnsuredBoldObject as TSomeClass);
  VerifyState(ObjB.M_parent, bvpsCurrent);
  ObjB.M_parent.Invalidate;
  VerifyState(ObjB.M_parent, bvpsInvalid);
  VerifyHasOldValues(ObjB.M_parent, true);
  FSubscriber.SubscribeToElement(ObjA.M_child);
  StoreValue(ObjB.M_parent);
  UndoHandler.SetCheckPoint('aCheckPoint');
  ObjA.child.Remove(ObjB); //modify-delete
  VerifyState(ObjB.M_parent, bvpsModified); //VerifyState(ObjB.M_parent, bvpsInvalid);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB.M_parent, GetStoredValueOfMember(ObjB.M_parent));
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyListAdjustedEx(ObjA.child, ObjB, false);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA.M_child);

  {ObjB.parent current}
  Prepare;
  ObjB := (ObjBLocator.EnsuredBoldObject as TSomeClass);
  ObjB.parent;
  VerifyState(ObjB.M_parent, bvpsCurrent);
  FSubscriber.SubscribeToElement(ObjA.M_child);
  FSubscriber.SubscribeToElement(ObjB.M_parent);
  StoreValue(ObjB.M_parent);
  ObjA.child.Remove(ObjB); //modify-delete
  VerifyIsNil(ObjB.M_parent);
  FSubscriber.VerifySendEvent(beValueChanged, ObjB.M_parent);
  VerifyState(ObjB.M_parent, bvpsModified);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB.M_parent, GetStoredValueofMember(objB.M_parent));
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyListAdjustedEx(ObjA.child, ObjB, false);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA.M_child);

  {ObjB.parent modified}
  Prepare;
  ObjB := (ObjBLocator.EnsuredBoldObject as TSomeClass);
  ObjB.parent := nil;
  ObjB.parent := ObjA;
  VerifyState(ObjB.M_parent, bvpsModified);
  FSubscriber.SubscribeToElement(ObjA.M_child);
  FSubscriber.SubscribeToElement(ObjB.M_parent);
  ObjA.child.Remove(ObjB); //modify-delete
  VerifyIsNil(ObjB.M_parent);
  VerifyState(ObjB.M_parent, bvpsModified);  //no state change
  FSubscriber.VerifySendEvent(beValueChanged, ObjB.M_parent);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB.M_parent, GetStoredValueofMember(ObjB.M_parent));
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyListAdjustedEx(ObjA.child, ObjB, false);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA.M_child);

  CloseAll;
end;

procedure Tmaan_ModifyTestCase.TestModifyNonEmbeddedRoleInsertTransient;
var
  ObjA1, ObjA2: TATransientClass;
  ObjB1, ObjB2: TAPersistentClass;

begin
  {ObjA.child Transient}
  SetTransientConfiguration;
  ObjB1 := FAPersistentClassList[0];
  ObjB2 := FAPersistentClassList[1];
  ObjA1 := FATransientClassList[0];
  ObjA2 := FATransientClassList[1];
  VerifyState(ObjB1.M_one, bvpsTransient);
  VerifyState(ObjA1.M_many, bvpsTransient);
  VerifyState(ObjA2.M_many, bvpsTransient);
  FSubscriber.SubscribeToElement(ObjA1.M_many);
  FSubscriber.SubscribeToElement(ObjB2.M_one);
  FSubscriber.SubscribeToElement(ObjA2.M_many);
  StoreValue(ObjB2.m_one);
  UndoHandler.SetCheckPoint('Check1');
  Assert.IsTrue(ObjB2.one = ObjA2);
  ObjA1.many.Add(ObjB2); //modify
  VerifyState(ObjA1.M_many, bvpsTransient);
  VerifyState(ObjA2.M_many, bvpsTransient);
  VerifyState(ObjB1.M_one, bvpsTransient);
  VerifyState(ObjB2.M_one, bvpsTransient);
  Assert.IsTrue(ObjB2.one = ObjA1);
  FSubscriber.VerifySendEvent(beValueChanged, ObjB2.M_one);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB2.M_one, GetStoredValueOfMember(ObjB2.M_one));
  VerifyListAdjustedIn(ObjA1.M_many, ObjB2, false);
  FSubscriber.VerifySendEvent(beItemAdded, ObjA1.M_many);
  VerifyListAdjustedEx(ObjA2.M_many, ObjB2, false);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA2.M_many);
  CloseAll;
end;

procedure Tmaan_ModifyTestCase.TestModifyNonEmbeddedRoleInsertCurrent;
var
  ObjA, ObjB, ObjA2: TSomeClass;
  ObjBLocator: TBoldObjectLocator;
  EmbeddedIndex: integer;
  procedure Prepare;
  begin
    FSomeClassList[1].parent := FSomeClassList[3];
    FSomeClassList[3].parent := FsomeClasslist[2];
    RefreshSystem;
    ObjA := FSomeClassList[0];
    ObjBLocator := FSomeClassList.Locators[1];
    ObjA2 := (FSomeClassList.Locators[2].EnsuredBoldObject as TSomeClass);
    ObjA.child.EnsureContentsCurrent;
    EmbeddedIndex := ObjA.M_parent.BoldMemberRTInfo.EmbeddedLinkIndex;
    VerifyState(ObjA.M_child, bvpsCurrent);
  end;

begin
  SetSimpleConfiguration;
  {ObjB.parent invalid}
  Prepare;
  FSubscriber.SubscribeToElement(ObjA.M_child);
  Assert.IsTrue(not Assigned(ObjBLocator.BoldObject));
  ObjA.child.AddLocator(ObjBLocator); //modify-insert
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState((ObjBLocator.BoldObject as TSomeClass).M_parent, bvpsModified);
  Assert.IsTrue(ObjA.child.LocatorInList(ObjBLocator), 'VerifyListAdjustedIn failed');
  FSubscriber.VerifySendEvent(beItemAdded, ObjA.M_child);

  {ObjB.parent invalid/adjust}
  Prepare;
  ObjB := ObjBLocator.EnsuredBoldObject as TSomeClass;
  VerifyState(ObjB.M_parent, bvpsCurrent);
  Assert.IsTrue(ObjB.parent <> nil);
  ObjB.M_parent.Invalidate;
  VerifyState(ObjB.M_parent, bvpsInvalid);
  VerifyHasOldValues(ObjB.M_parent, true);
  StoreValue(ObjB.M_parent);
  UndoHandler.SetCheckPoint('aCheckPoint');
  FSubscriber.SubscribeToElement(ObjA.M_child);
  ObjA.M_child.Add(ObjB); //modify-insert
  VerifyState(ObjB.M_parent, bvpsModified);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB.M_parent, GetStoredValueOfMember(ObjB.M_parent));
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyListAdjustedIn(ObjA.child, ObjB, false);
  FSubscriber.VerifySendEvent(beItemAdded, ObjA.M_child);

  {ObjB.parent current}
  Prepare;
  ObjB := ObjBLocator.EnsuredBoldObject as TSomeClass;
  ObjB.parent;
  VerifyState(ObjB.M_parent, bvpsCurrent);
  FSubscriber.SubscribeToElement(ObjA.M_child);
  FSubscriber.SubscribeToElement(ObjB.M_parent);
  StoreValue(ObjB.M_parent);
  ObjA.child.Add(ObjB); //modify
  VerifyState(ObjB.M_parent, bvpsModified);
  Assert.IsTrue(ObjB.M_parent.Locator = ObjA.BoldObjectlocator);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB.M_parent, GetStoredValueOfMember(ObjB.M_parent));
  FSubscriber.VerifySendEvent(beValueChanged, ObjB.M_parent);
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyListAdjustedIn(ObjA.child, ObjB, false);
  FSubscriber.VerifySendEvent(beItemAdded, ObjA.M_child);

  {ObjB.parent modified}
  Prepare;
  ObjB := ObjBLocator.EnsuredBoldObject as TSomeClass;
  Assert.IsTrue(objB.parent <> ObjA2);
  ObjB.parent := ObjA2;
  VerifyState(ObjB.M_parent, bvpsModified);
  FSubscriber.SubscribeToElement(ObjA.M_child);
  FSubscriber.SubscribeToElement(ObjB.M_parent);
  UndoHandler.SetCheckPoint('Block1');
  StoreValue(ObjB.M_parent);
  ObjA.child.Add(ObjB);  //modify
  Assert.IsTrue((ObjB.M_parent as TBoldObjectReference).Locator = ObjA.BoldObjectLocator);
  VerifyState(ObjB.M_parent, bvpsModified);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB.M_parent, GetStoredValueOfMember(ObjB.M_parent));
  FSubscriber.VerifySendEvent(beValueChanged, ObjB.M_parent);
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyListAdjustedIn(ObjA.child, ObjB, false);
  FSubscriber.VerifySendEvent(beItemAdded, ObjA.M_child);

  CloseAll;
end;

procedure Tmaan_ModifyTestCase.TestModifyNonEmbeddedRoleDeleteTransient;
var
  ObjA1: TATransientClass;
  ObjB1: TAPersistentClass;

begin
  {ObjA.child Transient}
  SetTransientConfiguration;
  ObjA1 := FATransientClassList[0];
  ObjB1 := FAPersistentClassList[0];
  VerifyState(ObjB1.M_one, bvpsTransient);
  VerifyState(ObjA1.M_many, bvpsTransient);
  FSubscriber.SubscribeToElement(ObjA1.M_many);
  FSubscriber.SubscribeToElement(ObjB1.M_one);
  StoreValue(ObjB1.m_one);
  UndoHandler.SetCheckPoint('Check1');
  ObjA1.many.Remove(ObjB1);  //modify
  VerifyState(ObjA1.M_many, bvpsTransient);
  VerifyState(ObjB1.M_one, bvpsTransient);
  VerifyIsNil(ObjB1.M_one);
  FSubscriber.VerifySendEvent(beValueChanged, ObjB1.M_one);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB1.M_one, GetStoredValueOfMember(ObjB1.M_one));
  VerifyListAdjustedEx(ObjA1.M_many, ObjB1, false);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA1.M_many);

  CloseAll;
end;

initialization
  TDUnitX.RegisterTestFixture(Tmaan_ModifyTestCase);

end.
