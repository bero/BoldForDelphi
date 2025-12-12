unit maan_Undo;

interface

uses
  DUnitX.TestFramework,
  maan_UndoRedoBase;

type
  [TestFixture]
  Tmaan_UndoTestCase = class(Tmaan_UndoRedoAbstractTestCase)
  public
    [Test]
    [Ignore('BoldUndoHandler.pas:305 - HandleMember passes RegardAsExisting=true, forcing besExisting state')]
    procedure UndoAttribute;                  {2.7b & 2.2a Undo block}
    [Test]
    [Ignore('BoldUndoHandler.pas:305 - HandleMember passes RegardAsExisting=true, forcing besExisting state')]
    procedure UndoEmbeddedRoleModified;       {2.7b & 2.2a Undo block}
    [Test]
    [Ignore('BoldUndoHandler.pas:305 - HandleMember passes RegardAsExisting=true, forcing besExisting state')]
    procedure UndoEmbeddedRoleTransient;      {2.7b & 2.2a Undo block}
    [Test]
    [Ignore('BoldUndoHandler.pas:305 - HandleMember passes RegardAsExisting=true, forcing besExisting state')]
    procedure UndoObjectCreation;             {2.7b & 2.2a Undo block}
//    procedure UndoObjectDeletion;             {2.7b & 2.2a Undo block}
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldDomainElement,
  BoldValueSpaceInterfaces,
  BoldSubscription,
  BoldUndoInterfaces,
  BoldId,
  BoldFreeStandingValues,
  UndoTestModelClasses,
  maan_UndoRedoTestCaseUtils;

{ Tmaan_UndoTestCase }

procedure Tmaan_UndoTestCase.UndoAttribute;
var
  SomeObject: TSomeClass;
  SomeObjectId: TBoldObjectId;
  TransientObject: TATransientClass;
  TransientObjectId: TBoldObjectId;
begin
  SetSimpleConfiguration;
  RefreshSystem;

  {transient attribute}
  TransientObject := CreateATransientClass(System, nil);
  TransientObjectId := TransientObject.BoldObjectLocator.BoldObjectId.Clone;
  UndoHandler.SetCheckPoint('UndoAttribute');
  StoreValue(TransientObject.M_aString);
  TransientObject.aString := TransientObject.aString + '123';
  VerifyState(TransientObject.M_aString, bvpsTransient);
  FSubscriber.SubscribeToElement(TransientObject.M_aString);
  UndoHandler.UnDoLatest; //Undo
  System.AssertLinkIntegrity;
  TransientObject := System.Locators.ObjectByID[TransientObjectId] as TATransientClass;
  TransientObject.ValuesAreEqual(TransientObject.M_aString.AsIBoldValue[bdepContents], GetStoredValueOfMember(TransientObject.M_aString), 'aString');
  VerifyState(TransientObject.M_aString, bvpsTransient);
  FSubscriber.VerifySendEvent(beValueChanged, TransientObject.M_aString);

  {Modified attribute}
  RefreshSystem;
  SomeObject := FSomeClassList[0];
  SomeObjectId := SomeObject.BoldObjectLocator.BoldObjectId.Clone;
  StoreValue(SomeObject.M_aString);
  VerifyState(SomeObject.M_aString, bvpsCurrent);
  SomeObject.aString := SomeObject.aString + '123';
  VerifyState(SomeObject.M_aString, bvpsModified);
  FSubscriber.SubscribeToElement(Someobject.M_aString);
  UndoHandler.UndoLatest;  //Undo
  System.AssertLinkIntegrity;
  SomeObject := System.Locators.ObjectById[SomeObjectId] as TSomeClass;
  VerifyState(SomeObject.M_aString, bvpsCurrent);
  Assert.IsTrue( SomeObject.ValuesAreEqual(SomeObject.M_aString.AsIBoldValue[bdepContents], GetStoredValueOfMember(SomeObject.M_aString), 'aString'));
  FSubscriber.VerifySendEvent(beValueChanged, SomeObject.M_aString);

  CloseAll;
end;

procedure Tmaan_UndoTestCase.UndoEmbeddedRoleModified;
var
  ObjA1, ObjA2, ObjB1, ObjB2: TSomeClass;
  ObjA1Id, ObjA2Id, ObjB1Id, ObjB2Id: TBoldObjectId;
  FSValue: TBFSObjectIdRef;
  procedure Prepare;
  begin
    RefreshSystem;
    FSomeClassList[1].parent := FSomeClasslist[0];
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
  {NonEmbeddedRole.parent invalid, ObjA1 invalid, ObjA2 invalid}
  FSValue := TBFSObjectIdRef.create;
  try
    StoreValue(ObjB1.M_parent);
    VerifyState(ObjB1.M_parent, bvpsCurrent);
    ObjB1.parent := ObjA2; // modify
    VerifyState(ObjA1.M_child, bvpsInvalid);
    VerifyState(ObjA2.M_child, bvpsInvalid);
    VerifyState(ObjB1.M_parent, bvpsModified);
    FSubscriber.SubscribeToElement(ObjB1.M_parent);
    VerifyState(ObjB1.M_parent, bvpsModified);
    FSValue.AssignContent(ObjB1.M_parent.AsIBoldValue[bdepContents]);
    UndoHandler.UndoLatest; // Undo
    System.AssertLinkIntegrity;
    SetObjectsFromIds;
    VerifyState(ObjB1.M_parent, bvpsCurrent);
    ObjB1.ValuesAreEqual(ObjB1.M_parent.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjB1.M_parent), 'parent');
    VerifyIsInRedoArea(ObjB1.M_parent, FSValue);
    FSubscriber.VerifySendEvent(beValueChanged, ObjB1.M_parent);
    VerifyState(ObjA2.M_child, bvpsinvalid);
    VerifyState(ObjA1.M_child, bvpsInvalid);
  finally
    FreeAndNil(FSValue);
  end;

  {NonEmbeddedRole.parent invalid, EmbeddeRole1 invalid, ObjA2 current}
  FSValue := TBFSObjectIdRef.create;
  try
    StoreValue(ObjB1.M_parent);
    VerifyState(ObjB1.M_parent, bvpsCurrent);
    ObjB1.parent := ObjA2; // modify
    VerifyState(ObjA1.M_child, bvpsInvalid);
    ObjA2.child.EnsureContentsCurrent;
    VerifyState(ObjA2.M_child, bvpsCurrent);
    VerifyState(ObjB1.M_parent, bvpsModified);
    FSubscriber.SubscribeToElement(ObjB1.M_parent);
    FSubscriber.SubscribeToElement(ObjA2.M_child);
    FSubscriber.SubscribeToElement(ObjA1.M_child);
    VerifyState(ObjB1.M_parent, bvpsModified);
    FSValue.AssignContent(ObjB1.M_parent.AsIBoldValue[bdepContents]);
    Assert.IsTrue(objB1.parent = ObjA2);
    UndoHandler.UndoLatest; // Undo
    System.AssertLinkIntegrity;
    SetObjectsFromIds;
    VerifyState(ObjB1.M_parent, bvpsCurrent);
    Assert.IsTrue(ObjB1.parent = ObjA1);
    VerifyIsInRedoArea(ObjB1.M_parent, FSValue);
    FSubscriber.VerifySendEvent(beValueChanged, ObjB1.M_parent);
    VerifyState(ObjA1.M_child, bvpsInvalid);
    VerifyState(ObjA2.M_child, bvpsCurrent);
    Assert.IsTrue(ObjA2.child.IndexOf(ObjB1) = -1);
    FSubscriber.VerifySendEvent(beItemDeleted, ObjA2.M_child);
  finally
    FreeAndNil(FSValue);
  end;

  {NonEmbeddedRole.parent current, NonEmbeddedRole in parent.child}
  Prepare;
  FSValue := TBFSObjectIdRef.create;
  try
    StoreValue(ObjB2.M_parent);
    VerifyState(ObjB2.M_parent, bvpsCurrent);
    ObjB2.parent := ObjA1; // modify
    VerifyState(ObjB2.M_parent, bvpsModified);
    ObjA1.child.EnsureContentsCurrent;
    ObjA2.child.EnsureContentsCurrent;
    VerifyState(ObjA1.M_child, bvpsCurrent);
    VerifyState(ObjA2.M_child, bvpsCurrent);
    FSubscriber.SubscribeToElement(ObjB2.m_parent);
    FSubscriber.SubscribeToElement(ObjA1.M_child);
    FSubscriber.SubscribeToElement(ObjA2.M_child);
    FSValue := TBFSObjectIdRef.create;
    FSValue.AssignContent(ObjB2.M_parent.AsIBoldValue[bdepContents]);
    UndoHandler.UndoLatest; //Undo
    System.AssertLinkIntegrity;
    SetObjectsFromIds;
    VerifyState(ObjB2.M_parent, bvpsCurrent);
    ObjB2.ValuesAreEqual(ObjB2.M_parent.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjB2.M_parent), 'parent');
    VerifyIsInRedoArea(ObjB2.M_parent, FSValue);
    FSubscriber.VerifySendEvent(beValueChanged, ObjB2.M_parent);
    VerifyListAdjustedEx(ObjA1.child, ObjB2, false);
    VerifyListAdjustedIn(ObjA2.child, ObjB2, false);
    FSubscriber.VerifySendEvent(beItemDeleted, ObjA1.M_child);
    FSubscriber.VerifySendEvent(beItemAdded, ObjA2.M_child);
  finally
    FreeAndNil(FSValue);
  end;

  Prepare;
  {NonEmbeddedRole.parent invalid/adjust, ObjA1 invalid, ObjA2 current}
  FSValue := TBFSObjectIdRef.create;
  try
    VerifyState(ObjB1.M_parent, bvpsCurrent);
    ObjB1.M_parent.Invalidate;
    VerifyState(ObjB1.M_parent, bvpsInvalid);
    VerifyHasOldValues(ObjB1.M_parent);
    StoreValue(ObjB1.M_parent);
    ObjA2.child.Add(ObjB1); // modify
    FSubscriber.SubscribeToElement(ObjB1.M_parent);
    VerifyState(ObjB1.M_parent, bvpsModified);
    FSValue.AssignContent(ObjB1.M_parent.AsIBoldValue[bdepContents]);
    UndoHandler.UndoLatest; // Undo
    System.AssertLinkIntegrity;
    SetObjectsFromIds;
    VerifyState(ObjB1.M_parent, bvpsInvalid);
    VerifyHasOldValues(ObjB1.M_parent, true);
    ObjB1.ValuesAreEqual(ObjB1.M_parent.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjB1.M_parent), 'parent');
    VerifyIsInRedoArea(ObjB1.M_parent, FSValue);
    FSubscriber.VerifySendEvent(beValueChanged, ObjB1.M_parent);
  finally
    FreeAndNil(FSValue);
  end;
end;

procedure Tmaan_UndoTestCase.UndoEmbeddedRoleTransient;
var
  ObjA1, ObjA2: TATransientClass;
  ObjB2: TAPersistentClass;
  ObjA1Id, ObjA2Id, ObjB2Id: TBoldObjectId;
  FSValueBeforeUndo: TBFSObjectIdRef;
  HUndo: IBoldUndoHandler;
  blockname: string;
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
  FSValueBeforeUndo := TBFSObjectIdRef.create;
  HUndo := (self.System.UndoHandler as IBoldUndoHandler);
  try
    blockname := HUndo.SetCheckPoint();
    StoreValue(ObjB2.M_one);
    ObjB2.one := ObjA1; // modify
    VerifyState(ObjB2.M_one, bvpsTransient);
    VerifyState(ObjA1.M_many, bvpsTransient);
    VerifyState(ObjA2.M_many, bvpsTransient);
    FSubscriber.SubscribeToElement(ObjB2.m_one);
    FSubscriber.SubscribeToElement(ObjA1.M_many);
    FSubscriber.SubscribeToElement(ObjA2.M_many);
    FSValueBeforeUndo := TBFSObjectIdRef.create;
    FSValueBeforeUndo.AssignContent(ObjB2.M_one.AsIBoldValue[bdepContents]);
    UndoHandler.UndoLatest; //Undo
    System.AssertLinkIntegrity;
    SetObjectsFromIds;
    VerifyState(ObjB2.M_one, bvpsTransient);
    ObjB2.ValuesAreEqual(ObjB2.M_one.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjB2.M_one), 'one');
    VerifyIsInRedoArea(ObjB2.M_one, FSValueBeforeUndo);
    FSubscriber.VerifySendEvent(beValueChanged, ObjB2.M_one);
    VerifyListAdjustedEx(ObjA1.many, ObjB2, false);
    VerifyListAdjustedIn(ObjA2.many, ObjB2, false);
    FSubscriber.VerifySendEvent(beItemDeleted, ObjA1.M_many);
    FSubscriber.VerifySendEvent(beItemAdded, ObjA2.M_many);
  finally
    FreeAndNil(FSValueBeforeUndo);
    HUndo := nil;
    CloseAll;
  end;
end;

procedure Tmaan_UndoTestCase.UndoObjectCreation;
var
  ObjA1, ObjB1: TSomeClass;
  ObjA1Id, ObjB1Id: TBoldObjectId;
  FSValue: TBFSObjectIdRef;
  HUndo: IBoldUndoHandler;
  blockName: string;
  procedure SetObjectsFromIds;
  begin
    ObjA1 := System.Locators.ObjectById[ObjA1Id] as TsomeClass;
    ObjB1 := System.Locators.ObjectById[ObjB1Id] as TsomeClass;
  end;
begin
  if not dmUndoRedo.BoldSystemHandle1.Active then
    dmUndoRedo.BoldSystemHandle1.Active := true;
  HUndo:= (Self.System.UndoHandler as IBoldUndoHandler);
  ObjA1 := CreateSomeClass(self.System, nil);
  UpdateDatabase;
  ObjA1Id := ObjA1.BoldObjectLocator.BoldObjectId.Clone;
  BlockName := HUndo.SetCheckPoint('Undo object creation');
  ObjB1 := CreateSomeClass(self.System, nil);
  ObjB1Id := ObjB1.BoldObjectLocator.BoldObjectId.Clone;
  ObjB1.parent := ObjA1;
  FSValue := TBFSObjectIdRef.create;
  try
    FSValue.AssignContent(ObjB1.M_parent.AsIBoldValue[bdepContents]);
    HUndo.UndoBlock(blockname); // undo
    SetObjectsFromIds;
    Assert.IsTrue(ObjA1.BoldExistenceState = besExisting);
    Assert.IsTrue( not Assigned(ObjB1));
    Assert.IsTrue(ObjA1.child.Count = 0);
    HUndo.RedoBlock(blockname); // redo
    SetObjectsFromIds;
    Assert.IsTrue(ObjA1.BoldExistenceState = besExisting);
    Assert.IsTrue(ObjB1.BoldExistenceState = besExisting);
    ObjB1.ValuesAreEqual(ObjB1.M_parent.AsIBoldValue[bdepContents], FSValue, 'parent');
    Assert.IsTrue(ObjA1.child.count = 1);
  finally
    FreeAndNil(FSValue);
    HUndo := nil;
    CloseAll;
  end;
end;

{
procedure Tmaan_UndoTestCase.UndoObjectDeletion;
begin

end;
}

initialization
  TDUnitX.RegisterTestFixture(Tmaan_UndoTestCase);

end.
