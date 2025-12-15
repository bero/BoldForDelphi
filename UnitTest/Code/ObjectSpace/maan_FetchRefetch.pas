unit maan_FetchRefetch;

interface

uses
  SysUtils,
  Classes,
  DUnitX.TestFramework,
  BoldDefs,
  BoldDomainElement,
  BoldSystem,
  BoldId,
  BoldSubscription,
  UndoTestModelClasses,
  BoldValueInterfaces,
  maan_UndoRedoBase,
  maan_UndoRedoTestCaseUtils;

type
  [TestFixture]
  [Category('FetchRefetch')]
  Tmaan_FetchRefetchTestCase = class(Tmaan_UndoRedoAbstractTestCase)
  public
    [Test]
    [Category('Slow')]
    procedure TestFetchInvalidAttribute;
    [Test]
    [Category('Slow')]
    procedure TestFetchCurrentAttribute;
    [Test]
    [Category('Slow')]
    procedure TestFetchModifiedAttribute;
    [Test]
    [Category('Slow')]
    [Ignore('Fix: Change bdepContents to bdepPMIn at BoldLinks.pas:742 TBoldDirectMultiLinkController.MakeDbCurrent')]
    procedure TestFetchEmbeddedRoleInvalid;
    [Test]
    [Category('Slow')]
    procedure TestFetchEmbeddedRoleInvalidAdjust;
    [Test]
    [Category('Slow')]
    procedure TestFetchEmbeddedRoleCurrent;
    [Test]
    [Category('Slow')]
    [Ignore('BoldUndoHandler.pas:305 - HandleMember passes RegardAsExisting=true, forcing besExisting state')]
    procedure TestFetchNonEmbeddedRoleInvalid;
  end;

implementation

{ Tmaan_FetchRefetchTestCase }

procedure Tmaan_FetchRefetchTestCase.TestFetchInvalidAttribute;
var
  SomeObject: TSomeClass;
  OId: TBoldObjectId;
begin
  Assert.IsNotNull(System, 'System should not be nil');
  GenerateObjects(System, 'SomeClass', 1);
  RefreshSystem;
  FSomeClassList.EnsureObjects;
  SomeObject := FSomeClassList[0] as TSomeClass;
  oid := SomeObject.BoldObjectLocator.BoldObjectId.Clone;
  try
    Someobject.M_aString.Invalidate;
    VerifyState(SomeObject.M_aString, bvpsInvalid );
    StoreObject(SomeObject);
    SomeObject := RefetchObject(System, Oid) as TSomeClass;
    VerifySetContents(SomeObject.AsIBoldObjectContents[bdepContents], GetStoredObjectContents(SomeObject));
    VerifyState(SomeObject.M_aString, bvpsCurrent);
  finally
    OId.Free;
  end;
end;

procedure Tmaan_FetchRefetchTestCase.TestFetchCurrentAttribute;
var
  SomeObject: TSomeClass;
begin
  GenerateObjects(System, 'SomeClass', 1);
  RefreshSystem;
  FSomeClassList.EnsureObjects;
  SomeObject := FSomeClassList[0];
  VerifyState(SomeObject.M_aString, bvpsCurrent);
  StoreValue(SomeObject.M_aString);
  OpenSystem2;
  FSomeClassList2.EnsureObjects;
  Assert.IsTrue(not FSomeclasslist2.Empty);
  FSomeclasslist2[0].aString := SomeObject.aString + '231';
  System2.UpdateDatabase;
  FSubscriber.SubscribeToElement(SomeObject.M_aString);
  SomeObject.M_aString.Refetch;
  VerifySetContents(SomeObject.AsIBoldObjectContents[bdepContents], GetStoredObjectContents(SomeObject));
  FSubscriber.VerifySendEvent(beValueChanged, SomeObject.M_aString);
  VerifyState(SomeObject.M_aString, bvpsCurrent);
end;

procedure Tmaan_FetchRefetchTestCase.TestFetchModifiedAttribute;
var
  SomeObject: TSomeClass;
  value: string;
begin
  GenerateObjects(System, 'SomeClass', 1);
  RefreshSystem;
  FSomeclassList.EnsureObjects;
  SomeObject := FSomeClassList[0];
  SomeObject.aString := SomeObject.aString + '123';
  value := SomeObject.aString;
  VerifyState(SomeObject.M_aString, bvpsModified);
  StoreValue(SomeObject.M_aString);
  SomeObject.M_aString.ReFetch;
  FSubscriber.SubscribeToElement(SomeObject);
  Assert.IsTrue(SomeObject.aString = value, 'TestFetchModifiedAttribute failed');
  VerifyState(SomeObject.M_aString, bvpsModified);
end;

procedure Tmaan_FetchRefetchTestCase.TestFetchEmbeddedRoleInvalid;
var
  ObjA: TSomeClass;
  ObjBLocator, NewObjBLocator: TBoldObjectLocator;
  objB: TSomeClass;
  NewObjB: TSomeClass;
  EmbeddedIndex: integer;
  Locator: TBoldObjectLocator;
  procedure prepare;
  begin
    RefreshSystem;
    ObjA := FSomeClassList[0];
    EmbeddedIndex := ObjA.M_parent.BoldMemberRTInfo.EmbeddedLinkIndex;
    ObjBLocator := FSomeClassList.Locators[1];
  end;
begin
//  TestClassListOrder;
  {ObjB.parent invalid}
  SetSimpleConfiguration;
  {ObjA.child Invalid}
  Prepare;
  VerifyState(ObjA.M_child, bvpsInvalid);
  ObjB := ObjBLocator.EnsuredBoldObject as TSomeClass; //fetch embedded role
  VerifyState(ObjB.M_parent, bvpsCurrent);
  Assert.IsTrue(ObjB.parent = ObjA, 'Line 134: ObjB.parent should = ObjA');

  {ObjA.child Current, and NewObjB not in ObjA.child}
  Prepare;
  NewObjBLocator := FSomeClassList.Locators[2];
  ObjA.Child.EnsureContentsCurrent;
  VerifyState(ObjA.M_child, bvpsCurrent);
  Locator := ObjBLocator.EmbeddedSingleLinks[EmbeddedIndex];
  Assert.IsTrue(Assigned(Locator), 'Line 142: Locator should be assigned');
  Assert.IsTrue(Locator.BoldObjectID.AsString = ObjA.BoldObjectLocator.BoldObjectID.AsString, 'Line 143: Locator ID should match ObjA ID');

  OpenSystem2;
  Assert.IsTrue(FSomeClassList.Locators[0].BoldObjectID.AsString = FSomeClassList2.Locators[0].BoldObjectID.AsString, 'Line 146: Locator[0] IDs should match');
  Assert.IsTrue(FSomeClassList.Locators[1].BoldObjectID.AsString = FSomeClassList2.Locators[1].BoldObjectID.AsString, 'Line 147: Locator[1] IDs should match');
  Assert.IsTrue(FSomeClassList.Locators[2].BoldObjectID.AsString = FSomeClassList2.Locators[2].BoldObjectID.AsString, 'Line 148: Locator[2] IDs should match');
  FSomeClassList2[0].child.Add(FSomeClassList2[2]);
  Assert.IsTrue(FSomeClassList2[2].Parent = FSomeClassList2[0], 'Line 150: FSomeClassList2[2].Parent should = FSomeClassList2[0]');
  SaveAndCloseSystem2;

  StoreObject(ObjA);
  FSubscriber.SubscribeToElement(ObjA.M_child);
  NewObjB := NewObjBLocator.EnsuredBoldObject as TSomeClass; //fetch embedded role
  VerifyState(NewObjB.M_parent, bvpsCurrent);
  Assert.IsTrue(NewObjB.parent = ObjA, 'Line 157: NewObjB.parent should = ObjA');
  Assert.IsTrue(not Assigned(NewObjBLocator.EmbeddedSingleLinks[EmbeddedIndex]), 'Line 158: NewObjBLocator.EmbeddedSingleLinks should not be assigned');
  VerifyListAdjustedIn(ObjA.child, NewObjB, true);
  FSubscriber.VerifySendEvent(beItemAdded, ObjA.child);

  {ObjA.child Current, and ObjB in ObjA.child}
  Prepare;
  ObjA.Child.EnsureContentsCurrent;
  VerifyState(ObjA.M_child, bvpsCurrent);
  Assert.IsTrue(not Assigned(ObjBLocator.BoldObject), 'Line 166: ObjBLocator.BoldObject should not be assigned');
  Assert.IsTrue(ObjBLocator.EmbeddedSingleLinks[EmbeddedIndex].BoldObjectID.AsString = ObjA.BoldObjectLocator.BoldObjectID.AsString, 'Line 167: EmbeddedSingleLinks ID should match ObjA ID');

  OpenSystem2;
  Assert.IsTrue(FSomeClassList.Locators[2].BoldObjectID.AsString = FSomeClassList2.Locators[2].BoldObjectID.AsString, 'Line 170: Locator[2] IDs should match in System2');
  FsomeClassList2[1].parent := nil;  // remove NonEmbeddedRole from ObjA.child
  SaveAndCloseSystem2;

  StoreObject(ObjA);
  FSubscriber.SubscribeToElement(ObjA.M_child);
  ObjB := ObjBLocator.EnsuredBoldObject as TSomeClass; //fetch embedded role
  objB.M_Parent.Refetch;
  VerifyState(ObjB.M_parent, bvpsCurrent);
  Assert.IsTrue(ObjB.parent = nil, 'Line 179: ObjB.parent should be nil');
  VerifyListAdjustedEx(ObjA.child, ObjB , true);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA.child);
end;

procedure Tmaan_FetchRefetchTestCase.TestFetchEmbeddedRoleInvalidAdjust;
var
  ObjA, ObjB: TSomeClass;
  NewObjALocator: TBoldObjectLocator;
  procedure prepare;
  begin
    RefreshSystem;
    ObjA := FSomeClassList[0];
    ObjB := FSomeClassList[1];
    NewObjALocator := FSomeClassList.Locators[2];
    ObjB.M_parent.Invalidate;
    VerifyState(ObjB.M_parent, bvpsInvalid);
    VerifyHasOldValues(ObjB.M_parent, true);
  end;

  procedure ModifyEmbeddedStateInDb;
  begin
    OpenSystem2;
    FSomeClassList2[1].parent := FSomeClassList2[2];
    SaveAndCloseSystem2;
  end;

  procedure UndoModifyEmbeddedStateInDb;
  begin
    //Undo the changes
    OpenSystem2;
    FSomeClassList2[1].parent := FSomeClassList2[0];
    SaveAndCloseSystem2;
  end;

begin
  SetSimpleConfiguration;
  { ObjA.child Invalid, and ObjB.parent not modified after fetch }
  Prepare;
  ObjB.M_parent.EnsureContentsCurrent;
  VerifyState(ObjA.M_child, bvpsInvalid);
  Assert.IsTrue(ObjB.parent = ObjA);
  VerifyState(ObjB.M_parent, bvpsCurrent);
  VerifyState(ObjA.M_child, bvpsInvalid);

  {ObjA.child Invalid, and ObjB.parent modified after fetch }
  Prepare;
  VerifyState(ObjB.M_child, bvpsInvalid);
  VerifyState((NewObjALocator.EnsuredBoldObject as TSomeClass).M_child, bvpsInvalid);

  ModifyEmbeddedStateInDb;
  ObjB.M_parent.Refetch;
  Assert.IsTrue(ObjB.parent = (NewObjALocator.BoldObject as TSomeClass));
  VerifyState(ObjB.M_parent, bvpsCurrent);
  VerifyState(ObjA.M_child, bvpsInvalid);
  VerifyState((NewObjALocator.BoldObject as TSomeClass).M_child, bvpsInvalid);
  UndoModifyEmbeddedStateInDb;

  {ObjA.child Invalid, NewObjA.child current, NewObjB current}
  Prepare;
  (NewObjALocator.EnsuredBoldObject as TSomeClass).child.EnsureContentsCurrent;
  VerifyState(ObjB.M_child, bvpsInvalid);
  VerifyState((NewObjALocator.EnsuredBoldObject as TSomeClass).M_child, bvpsCurrent);

  ModifyEmbeddedStateInDb;
  FSubscriber.SubscribeToElement((NewObjALocator.BoldObject as TSomeClass).M_child);
  ObjB.M_parent.Refetch;
  Assert.IsTrue(ObjB.parent = (NewObjALocator.BoldObject as TSomeClass));
  VerifyState(ObjB.M_parent, bvpsCurrent);
  VerifyState(ObjA.M_child, bvpsInvalid);
  VerifyState((NewObjALocator.BoldObject as TSomeClass).M_child, bvpsCurrent);
  VerifyListAdjustedIn((NewObjALocator.BoldObject as TSomeClass).child, ObjB, true);
  FSubscriber.VerifySendEvent(beItemAdded, (NewObjALocator.BoldObject as TSomeClass).M_child);
  UndoModifyEmbeddedStateInDb;

  {ObjA.child Current, Old(ObjA = New(ObjB) }
  Prepare;
  ObjA.child.EnsureContentsCurrent;
  VerifyState(ObjA.M_child, bvpsCurrent);

  StoreValue(ObjB.M_parent);
  StoreValue(ObjA.M_child);
  ObjB.M_parent.EnsureCOntentsCurrent;
  VerifySetContents(ObjB.M_parent.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjB.M_parent));
  VerifyState(ObjB.M_parent, bvpsCurrent);

  VerifyState(ObjA.M_child, bvpsCurrent);
  Assert.IsTrue(ObjA.M_child.Count = 1, 'TestFetchEmbeddedRoleInvalidAdjust failed');
  Assert.IsTrue((GetStoredValueOfMember(ObjA.M_child) as IBoldObjectIdListRef).Count = 1, 'TestFetchEmbeddedRoleInvalidAdjust failed');
  Assert.IsTrue(ObjA.M_child[0].BoldObjectLocator.BoldObjectId.AsString = (GetStoredValueOfMember(ObjA.M_child) as IBoldObjectIdListRef).IdList[0].ASString, 'TestFetchEmbeddedRoleInvalidAdjust failed');

  {ObjA.child Current, NewObjB current, and ObjA <> NewObjA}
  Prepare;
  (NewObjALocator.EnsuredBoldObject as TSomeClass).child.EnsureContentsCurrent;
  ObjA.child.EnsureContentsCurrent;
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState((NewObjALocator.BoldObject as TSomeClass).M_child, bvpsCurrent);

  ModifyEmbeddedStateInDb;
  StoreValue(ObjB.M_parent);
  StoreValue((NewObjALocator.BoldObject as TSomeClass).M_child);
  StoreValue(ObjA.M_child);
  FSubscriber.SubscribeToElement((NewObjALocator.BoldObject as TSomeClass).M_child);
  FSubscriber.SubscribeToElement(ObjA.M_child);
  ObjB.M_parent.Refetch;
  VerifySetContents(ObjB.M_parent.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjB.M_parent));
  VerifyState(ObjB.M_parent, bvpsCurrent);
  VerifyState(ObjA.M_child, bvpsCurrent);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA.M_child);
  VerifyListAdjustedEx(ObjA.child, ObjB, true);
  VerifyState((NewObjALocator.BoldObject as TSomeClass).M_child, bvpsCurrent);
  FSubscriber.VerifySendEvent(beItemAdded, (NewObjALocator.BoldObject as TSomeClass).M_child);
  VerifyListAdjustedIn((NewObjALocator.BoldObject as TSomeClass).child, ObjB, true);
  UndoModifyEmbeddedStateInDb;
end;

procedure Tmaan_FetchRefetchTestCase.TestFetchEmbeddedRoleCurrent;
var
  ObjA, ObjB: TSomeClass;
  msg: string;
  procedure prepare;
  begin
    RefreshSystem;
    ObjA := FSomeClassList[0];
    ObjB := FSomeClassList[1];
    VerifyState(ObjB.M_parent, bvpsCurrent);
  end;

  procedure ModifyEmbeddedStateInDb;
  begin
    OpenSystem2;
    FSomeClassList2[1].parent := FSomeClassList2[2];
    SaveAndCloseSystem2;
  end;

  procedure UndoModifyEmbeddedStateInDb;
  begin
    OpenSystem2;
    FSomeClassList2[1].parent := FSomeClassList2[0];
    SaveAndCloseSystem2;
  end;

  procedure FetchEmbeddedRole;
  begin
    ObjB.BoldObjectLocator.EnsureBoldObject;
  end;

begin
  SetSimpleConfiguration;
  {ObjB Invalid, and Old(ObjB = New(ObjB) }
  Prepare;
  VerifyState(ObjA.M_child, bvpsInvalid);
  ModifyEmbeddedStateInDb;
  StoreValue(ObjB.M_parent);
  FSubscriber.SubscribeToElement(ObjB.M_parent);
  ObjB.M_parent.Refetch;
  msg := 'Member is not fetch unless its state is invalid';
  Assert.IsTrue(ObjB.parent = FSomeClassList[2], msg);
  VerifyState(ObjB.M_parent, bvpsCurrent);
  FSubscriber.VerifySendEvent(beValueChanged, ObjB.M_parent);
  VerifyState(ObjA.M_child, bvpsInvalid);
  Exit;
end;

procedure Tmaan_FetchRefetchTestCase.TestFetchNonEmbeddedRoleInvalid;
var
  ObjA, ObjB, ObjA2, ObjB2: TSomeClass;
  ObjBLocator: TBoldObjectLocator;
  EmbeddedIndex: integer;

  procedure prepare;
  begin
    if not dmUndoRedo.BoldSystemHandle1.Active then
      dmUndoRedo.BoldSystemHandle1.Active := true;
    FSomeClassList[0].child.Clear;
    FSomeClassList[1].child.Clear;
    FSomeClassList[2].child.Clear;
    dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
    Assert.IsTrue(fSomeClasslist[1].parent = nil);
    FSomeClassList[1].parent := FSomeClassList[0];
    Assert.IsTrue(fSomeClasslist[3].parent = nil);
    FSomeClassList[3].parent := FSomeClassList[2];
    dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
    RefreshSystem;
    ObjA := FSomeClassList[0];
    ObjBLocator := FSomeClassList.locators[1];
    ObjA2 := FSomeClassList[2];
    ObjB2 := FSomeClassList[3];
    VerifyState(ObjA.M_child, bvpsInvalid);
    EmbeddedIndex := ObjA.M_parent.BoldMemberRTInfo.EmbeddedLinkIndex ;
  end;

  procedure ModifyEmbeddedStateInDb;
  begin
    OpenSystem2;
    FSomeClassList2[3].parent := FSomeClassList2[0];
    SaveAndCloseSystem2;
  end;

begin
  SetSimpleConfiguration;
  {ObjB.parent invalid}
  Prepare;
  StoreValue(ObjA.M_child);
  ObjA.child.EnsureContentsCurrent;
  VerifySetContents(ObjA.M_child.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjA.M_child));
  VerifyState(ObjA.M_child, bvpsCurrent);
  Assert.IsTrue(Assigned(ObjBLocator.EmbeddedSingleLinks[EmbeddedIndex]));
  Assert.IsTrue(ObjBLocator.EmbeddedSingleLinks[EmbeddedIndex].BoldObjectID.AsString = ObjA.BoldObjectLocator.BoldObjectID.AsString);

  {ObjB.parent Invalid/Adjust}
  {after fetch ObjB not included in ObjA.child}
  Prepare;
  ObjB := (ObjBLocator.EnsuredBoldObject as TSomeClass);
  Assert.IsTrue(objB.Parent <> nil);
  ObjB.M_parent.Invalidate;
  StoreValue(ObjA.M_child);
  VerifyState(ObjB.M_parent, bvpsInvalid);
  VerifyHasOldValues(ObjB.M_parent);

  //change in DB
  OpenSystem2;
  FSomeClassList2.EnsureObjects;

  Assert.IsTrue(FSomeClassList2[1].BoldObjectLocator.BoldObjectId.AsString = ObjB.BoldObjectLocator.BoldObjectId.AsString);
  Assert.IsTrue(FSomeClassList2[1].parent.BoldObjectLocator.BoldObjectId.AsString = ObjA.BoldObjectLocator.BoldObjectId.AsString);
  FSomeClassList2[1].parent := nil;
  System2.UpdateDatabase;
  SaveAndCloseSystem2;

  ObjA.child.EnsureContentsCurrent;
  VerifySetContents(ObjA.M_child.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjA.M_child));
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState(ObjB.M_parent, bvpsInvalid);
  VerifyHasOldValues(ObjB.M_parent, false);


  {ObjB.parent Invalid/Adjust}
  {after fetch ObjB.parent not modified}
  Prepare;
  ObjB := (ObjBLocator.EnsuredBoldObject as TSomeClass);
  ObjB.M_parent.Invalidate;
  VerifyState(ObjB.M_parent, bvpsInvalid);
  VerifyHasOldValues(ObjB.M_parent);
  StoreValue(ObjA.M_child);
  ObjA.child.EnsureContentsCurrent;
  VerifySetContents(ObjA.M_child.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjA.M_child));
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState(ObjB.M_parent, bvpsInvalid); // Fetching multi end should not effect invalid single end

  {ObjB2.parent Invalid/Adjust}
  {after fetch ObjB2 included in ObjA.child}
  Prepare;
  VerifyState(ObjA2.M_child,bvpsInvalid);
  VerifyState(ObjB2.M_parent, bvpsCurrent);
  ObjA2.M_child.EnsureContentsCurrent;  //fetch
  VerifyState(ObjA2.M_child, bvpsCurrent);
  VerifyState(ObjB2.M_parent, bvpsCurrent);
  Assert.IsTrue(ObjB2.parent = ObjA2);
  ObjB2.Invalidate;
  VerifyState(ObjB2.M_parent, bvpsInvalid);
  VerifyHasOldValues(ObjB2.M_parent);
  VerifyState(ObjA.M_child, bvpsInvalid);
  FSubscriber.SubscribeToElement(ObjA2.M_child);

  //change in DB
  OpenSystem2;
  FSomeClasslist2.EnsureObjects;
  FSomeClassList2[3].parent := FSomeClassList2[0];
  System2.UpdateDatabase;
  SaveAndCloseSystem2;

  VerifyState(ObjA.M_child, bvpsInvalid);
  VerifyState(ObjB2.M_parent, bvpsInvalid);
  VerifyHasOldValues(ObjB2.M_parent);
//  Assert.IsTrue((ObjB2.M_parent as TBoldObjectReference).Locator = ObjA2.BoldObjectLocator);
  Assert.IsTrue((ObjB2.M_parent.asIBoldValue[bdepContents] as IBoldObjectIdRef).id.AsString = ObjA2.BoldObjectLocator.BoldObjectId.asstring);
  VerifyState(ObjA2.M_child, bvpsCurrent);
  Assert.IsTrue(ObjA2.M_child.indexOf(ObjB2) <> -1);
  ObjA.child.EnsureContentsCurrent;

  Assert.IsTrue(ObjA.child.count = 2);
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState(ObjB2.M_parent, bvpsInvalid);
  Assert.IsTrue(ObjB2.parent = ObjA);
  VerifyListAdjustedEx(ObjA2.M_child, ObjB2, true);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA2.M_child);

  {ObjB.parent Current}
  {after fetch ObjB not included in ObjA.child}
  Prepare;
  ObjB := (ObjBLocator.EnsuredBoldObject as TSomeClass);
  VerifyState(ObjB.M_parent, bvpsCurrent);
  StoreValue(ObjA.M_child);
  //change in DB
  OpenSystem2;
  FsomeClassList2.EnsureObjects;
  FSomeClassList2[1].parent := nil;
  System2.UpdateDatabase;
  SaveAndCloseSystem2;

  ObjA.child.EnsureContentsCurrent;
  VerifySetContents(ObjA.M_child.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjA.M_child));
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState(ObjB.M_parent, bvpsInvalid);

  {ObjB.parent Current}
  {after fetch ObjB not modified}
  Prepare;
  ObjB := (ObjBLocator.EnsuredBoldObject as TSomeClass);
  VerifyState(ObjB.M_parent, bvpsCurrent);
  StoreValue(ObjA.M_child);
  ObjA.child.EnsureContentsCurrent;
  VerifySetContents(ObjA.M_child.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjA.M_child));
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState(ObjB.M_parent, bvpsCurrent);

  {ObjB.parent Current}
  {after fetch ObjB2 included in ObjA.child}
  Prepare;
  ObjA2.M_child.EnsureContentsCurrent;
  VerifyState(ObjA2.M_child, bvpsCurrent);
  VerifyState(ObjB2.M_parent, bvpsCurrent);
  StoreValue(ObjB2.M_parent);
  StoreValue(ObjA.M_child);
  FSubscriber.SubscribeToElement(ObjA2.M_child);
  FSubscriber.SubscribeToElement(ObjB2.M_parent);
  //change in DB
  OpenSystem2;
  FSomeClassList2.EnsureObjects;
  Assert.IsTrue(fSomeClassList2[0].BoldObjectLocator.BoldObjectId.asstring = ObjA.BoldObjectLocator.BoldObjectId.asstring);
  Assert.IsTrue(fSomeClassList2[3].BoldObjectLocator.BoldObjectId.asstring = ObjB2.BoldObjectLocator.BoldObjectId.asstring);
  FSomeClassList2[3].parent := FSomeClassList2[0];
  System2.UpdateDatabase;
  SaveAndCloseSystem2;
  ObjA.child.ensureContentsCurrent;
  VerifySetContents(ObjA.M_child.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjA.M_child));
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifySetContents(ObjB2.M_parent.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjB2.M_parent));
  VerifyState(ObjB2.M_parent, bvpsCurrent);
  Assert.IsTrue(ObjB2.parent = ObjA);
  FSubscriber.VerifySendEvent(beValueChanged, ObjB2.M_parent);
  VerifyListAdjustedEx(ObjA2.M_child, ObjB2, true);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA2.M_child);

end;

initialization
  TDUnitX.RegisterTestFixture(Tmaan_FetchRefetchTestCase);

end.
