unit Test.BoldFreeStandingValues;

interface

uses
  BoldTestFramework,
  BoldFreeStandingValues,
  BoldId,
  BoldValueInterfaces;

type
  // Test helper subclass that exposes protected methods for testing
  TTestableObjectIdRefPair = class(TBFSObjectIdRefPair)
  public
    procedure TestApplyTranslationList(TranslationList: TBoldIdTranslationList);
  end;

  { Tests for TBoldFreeStandingValueSpace and TBoldFreeStandingObjectContents }
  {$IFNDEF FPC}
  [Category('FreeStandingValues')]
  {$ENDIF}
  TTestBoldFreeStandingValueSpace = class(TBoldTestCase)
  private
    function HasValueAt(OC: TBoldFreeStandingObjectContents; Index: Integer): Boolean;
  published
    // TBoldFreeStandingValueSpace tests
    procedure TestValueSpaceCreate;
    procedure TestValueSpaceIsEmptyInitially;
    procedure TestValueSpaceIdCountZeroInitially;
    procedure TestValueSpaceEnsureObjectContents;
    procedure TestValueSpaceGetHasContentsForId;
    procedure TestValueSpaceGetFSObjectContentsByObjectId;
    procedure TestValueSpaceRemoveFSObjectContents;
    procedure TestValueSpaceAllObjectIds;
    procedure TestValueSpaceClear;
    procedure TestValueSpaceGetAnyObjectId;
    procedure TestValueSpaceContentType;

    // TBoldFreeStandingObjectContents tests
    procedure TestObjectContentsCreate;
    procedure TestObjectContentsIsEmptyInitially;
    procedure TestObjectContentsMemberCountZero;
    procedure TestObjectContentsEnsureMemberAndGetValueByIndex;
    procedure TestObjectContentsGetValueByIndex;
    procedure TestObjectContentsExistenceState;
    procedure TestObjectContentsPersistenceState;
    procedure TestObjectContentsTimeStamp;
    procedure TestObjectContentsMarkAllMembersCurrent;
    procedure TestObjectContentsRemoveMemberByIndex;
  end;

  { Tests for nullable value types (TBFSInteger, TBFSString, etc.) }
  {$IFNDEF FPC}
  [Category('FreeStandingValues')]
  {$ENDIF}
  TTestBoldFreeStandingNullableValues = class(TBoldTestCase)
  published
    // TBFSInteger tests
    procedure TestIntegerCreate;
    procedure TestIntegerSetAndGet;
    procedure TestIntegerIsNullInitially;
    procedure TestIntegerSetToNonNull;
    procedure TestIntegerContentType;
    procedure TestIntegerIsEqualToValue;
    procedure TestIntegerAssignContent;

    // TBFSString tests
    procedure TestStringCreate;
    procedure TestStringSetAndGet;
    procedure TestStringIsNullInitially;
    procedure TestStringContentType;
    procedure TestStringIsEqualToValue;
    procedure TestStringAssignContent;

    // TBFSFloat tests
    procedure TestFloatCreate;
    procedure TestFloatSetAndGet;
    procedure TestFloatContentType;
    procedure TestFloatIsEqualToValue;

    // TBFSCurrency tests
    procedure TestCurrencyCreate;
    procedure TestCurrencySetAndGet;
    procedure TestCurrencyContentType;

    // TBFSBoolean tests
    procedure TestBooleanCreate;
    procedure TestBooleanSetAndGet;
    procedure TestBooleanContentType;

    // TBFSDateTime tests
    procedure TestDateTimeCreate;
    procedure TestDateTimeSetAndGet;
    procedure TestDateTimeContentType;

    // TBFSDate tests
    procedure TestDateCreate;
    procedure TestDateSetAndGet;
    procedure TestDateContentType;

    // TBFSTime tests
    procedure TestTimeCreate;
    procedure TestTimeSetAndGet;
    procedure TestTimeContentType;

    // TBFSBlob tests
    procedure TestBlobCreate;
    procedure TestBlobSetAndGet;
    procedure TestBlobContentType;

    // TBFSTypedBlob tests
    procedure TestTypedBlobCreate;
    procedure TestTypedBlobContentTypeProperty;
    procedure TestTypedBlobContentType;

    // TBFSAnsiString tests
    procedure TestAnsiStringSetAndGet;
    procedure TestAnsiStringContentType;
    procedure TestAnsiStringAssignContent;
    procedure TestAnsiStringIsEqualToValue;
    procedure TestAnsiStringGetContentAsString;

    // TBFSUnicodeString tests
    procedure TestUnicodeStringSetAndGet;
    procedure TestUnicodeStringContentType;
    procedure TestUnicodeStringAssignContent;
    procedure TestUnicodeStringIsEqualToValue;

    // AssignContent tests for remaining types
    procedure TestCurrencyAssignContent;
    procedure TestCurrencyIsEqualToValue;
    procedure TestFloatAssignContent;
    procedure TestBooleanAssignContent;
    procedure TestBooleanIsEqualToValue;
    procedure TestDateTimeAssignContent;
    procedure TestDateTimeIsEqualToValue;
    procedure TestDateAssignContent;
    procedure TestDateIsEqualToValue;
    procedure TestTimeAssignContent;
    procedure TestTimeIsEqualToValue;
    procedure TestBlobAssignContent;
    procedure TestBlobIsEqualToValue;
    procedure TestTypedBlobAssignContent;
    procedure TestTypedBlobIsEqualToValue;

    // GetStringRepresentation / GetAsVariant
    procedure TestNullableGetStringRepresentationNull;
    procedure TestNullableGetStringRepresentationNonNull;
    procedure TestNullableGetAsVariantNull;
    procedure TestNullableGetAsVariantNonNull;

    // GetStreamName tests via IBoldStreamable
    procedure TestIntegerGetStreamName;
    procedure TestStringGetStreamName;
    procedure TestAnsiStringGetStreamName;
    procedure TestUnicodeStringGetStreamName;
    procedure TestCurrencyGetStreamName;
    procedure TestFloatGetStreamName;
    procedure TestBooleanGetStreamName;
    procedure TestDateTimeGetStreamName;
    procedure TestDateGetStreamName;
    procedure TestTimeGetStreamName;
    procedure TestBlobGetStreamName;
    procedure TestTypedBlobGetStreamName;

    // GetValueAsVariant via IBoldVariantReadable
    procedure TestStringGetValueAsVariant;
    procedure TestCurrencyGetValueAsVariant;
    procedure TestFloatGetValueAsVariant;
    procedure TestBooleanGetValueAsVariant;
    procedure TestDateTimeGetValueAsVariant;
    procedure TestDateGetValueAsVariant;
    procedure TestTimeGetValueAsVariant;
    procedure TestBlobGetValueAsVariant;
    procedure TestAnsiStringGetValueAsVariant;
    procedure TestUnicodeStringGetValueAsVariant;

    // AssignContentValue null paths
    procedure TestStringAssignContentNull;
    procedure TestAnsiStringAssignContentNull;
    procedure TestUnicodeStringAssignContentNull;
    procedure TestIntegerAssignContentNull;
    procedure TestCurrencyAssignContentNull;
    procedure TestFloatAssignContentNull;
    procedure TestBooleanAssignContentNull;
    procedure TestDateTimeAssignContentNull;
    procedure TestDateAssignContentNull;
    procedure TestTimeAssignContentNull;
    procedure TestBlobAssignContentNull;
    procedure TestTypedBlobAssignContentNull;

    // ContentAsString on nullable base
    procedure TestNullableBaseGetContentAsString;
  end;

  { Tests for TBFSObjectIdRef and list types }
  {$IFNDEF FPC}
  [Category('FreeStandingValues')]
  {$ENDIF}
  TTestBoldFreeStandingIdRefs = class(TBoldTestCase)
  published
    // TBFSObjectIdRef tests
    procedure TestObjectIdRefCreate;
    procedure TestObjectIdRefSetFromId;
    procedure TestObjectIdRefSetFromIdAdopt;
    procedure TestObjectIdRefContentType;
    procedure TestObjectIdRefGetStringRepresentation;
    procedure TestObjectIdRefIsEqualToValue;
    procedure TestObjectIdRefAssignContent;

    // TBFSObjectIdListRef tests
    procedure TestObjectIdListRefCreate;
    procedure TestObjectIdListRefCountZero;
    procedure TestObjectIdListRefSetFromIdList;
    procedure TestObjectIdListRefContentType;
    procedure TestObjectIdListRefAddAndRemoveId;

    // TBFSObjectIdListRefPair tests
    procedure TestObjectIdListRefPairCreate;
    procedure TestObjectIdListRefPairSetFromIdLists;
    procedure TestObjectIdListRefPairContentType;
    procedure TestObjectIdListRefPairAddIds;
  end;

  {$IFNDEF FPC}
  [Category('FreeStandingValues')]
  {$ENDIF}
  TTestBoldFSValueSpaceExtended = class(TBoldTestCase)
  published
    procedure TestRemoveDeletedObjects;
    procedure TestMarkAllObjectsAndMembersCurrent;
    procedure TestRemoveAllObjectContents;
    procedure TestClearWhenObjectContentsEmpty;
    procedure TestClearWhenObjectContentsNotEmpty;
    procedure TestGetValueForIdAndMemberIndex;
    procedure TestGetValueForIdAndMemberIndexNotFound;
    procedure TestAllObjectIdsOnlyLoaded;
    procedure TestApplyValueSpace;
    procedure TestRemoveFSObjectContentsDirect;
    procedure TestGetEnsuredObjectContentsByObjectIdAndCheckIfCreated;
    procedure TestAssertLinkIntegrity;
    procedure TestUpdateOwnValuesFrom;
  end;

  {$IFNDEF FPC}
  [Category('FreeStandingValues')]
  {$ENDIF}
  TTestBoldFSObjectContentsExtended = class(TBoldTestCase)
  published
    procedure TestGetValueByMemberId;
    procedure TestGetFSValueByIndex;
    procedure TestGetFSValueByIndexOutOfRange;
    procedure TestGetIsModified;
    procedure TestIsReadOnly;
    procedure TestGlobalId;
    procedure TestObjectContentsContentType;
    procedure TestGetStreamName;
    procedure TestIsEmptyWithMembers;
    procedure TestApplyObjectContentsIgnorePersistenceState;
    procedure TestObjectIdRefApplyTranslationList; // tests SetFromId replace
    procedure TestObjectIdListRefApplyTranslationList; // tests GetContentAsString
    procedure TestObjectIdRefGetContentAsString;
    procedure TestObjectIdRefGetStringRepresentationNil;
    procedure TestObjectIdListRefGetStringRepresentation;
    procedure TestObjectIdListRefSetList;
    procedure TestObjectIdListRefPairGetIdLists;
    procedure TestObjectIdListRefPairRemoveId;
    procedure TestObjectIdListRefPairAssignContent;
    procedure TestObjectIdListRefAssignContent;
    procedure TestGetObjectId;
    procedure TestUpdateObjectContentsFrom;
    procedure TestObjectIdRefGetStreamName;
    procedure TestObjectIdListRefGetStreamName;
    procedure TestObjectIdListRefPairGetStreamName;
  end;

  {$IFNDEF FPC}
  [Category('FreeStandingValues')]
  {$ENDIF}
  TTestBoldFreeStandingValues = class(TBoldTestCase)
  published
    // Basic empty state tests
    procedure TestObjectIdRefPairGetId1Empty;
    procedure TestObjectIdRefPairGetId2Empty;

    // SetFromIds tests
    procedure TestObjectIdRefPairGetId1WithOneId;
    procedure TestObjectIdRefPairGetId1And2WithTwoIds;
    procedure TestSetFromIdsVerifyActualIdValues;
    procedure TestSetFromIdsClonesIds;
    procedure TestSetFromIdsReplacesExistingIds;
    procedure TestSetFromIdsWithBothNil;

    // OrderNo tests
    procedure TestOrderNoDefaultValue;
    procedure TestOrderNoSetAndGet;

    // ContentType and StreamName tests
    procedure TestContentTypeReturnsObjectIdRefPair;
    procedure TestGetStreamNameReturnsCorrectValue;

    // AssignContentValue tests
    procedure TestAssignContentValueCopiesIds;
    procedure TestAssignContentValueCopiesOrderNo;
    procedure TestAssignContentValueWithNilIds;

    // ApplyTranslationList tests
    procedure TestApplyTranslationListTranslatesIds;
    procedure TestApplyTranslationListWithNoMatchingIds;
    procedure TestApplyTranslationListWithNilIdList;

    // GetStringRepresentation and GetContentAsString tests (bug fix tests)
    procedure TestGetStringRepresentationWithTwoIds;
    procedure TestGetStringRepresentationWithOneId;
    procedure TestGetStringRepresentationEmpty;
    procedure TestGetContentAsString;
    procedure TestIBoldStringRepresentableInterface;
  end;

implementation

uses
  SysUtils,
  Variants,
  BoldDefs,
  BoldStreams,
  BoldValueSpaceInterfaces,
  BoldDefaultStreamNames;

{ TTestBoldFreeStandingValueSpace }

procedure TTestBoldFreeStandingValueSpace.TestValueSpaceCreate;
var
  VS: TBoldFreeStandingValueSpace;
begin
  VS := TBoldFreeStandingValueSpace.Create;
  try
    Assert.IsNotNull(VS, 'ValueSpace should be created');
  finally
    VS.Free;
  end;
end;

procedure TTestBoldFreeStandingValueSpace.TestValueSpaceIsEmptyInitially;
var
  VS: TBoldFreeStandingValueSpace;
begin
  VS := TBoldFreeStandingValueSpace.Create;
  try
    Assert.IsTrue(VS.IsEmpty, 'ValueSpace should be empty initially');
  finally
    VS.Free;
  end;
end;

procedure TTestBoldFreeStandingValueSpace.TestValueSpaceIdCountZeroInitially;
var
  VS: TBoldFreeStandingValueSpace;
begin
  VS := TBoldFreeStandingValueSpace.Create;
  try
    Assert.AreEqual(0, VS.IdCount, 'IdCount should be 0 initially');
  finally
    VS.Free;
  end;
end;

procedure TTestBoldFreeStandingValueSpace.TestValueSpaceEnsureObjectContents;
var
  VS: TBoldFreeStandingValueSpace;
  ObjectId: TBoldObjectId;
begin
  VS := TBoldFreeStandingValueSpace.Create;
  try
    ObjectId := TBoldInternalObjectId.CreateWithClassID(0, True);
    try
      VS.EnsureObjectContents(ObjectId);
      Assert.AreEqual(1, VS.IdCount, 'IdCount should be 1 after EnsureObjectContents');
      Assert.IsFalse(VS.IsEmpty, 'ValueSpace should not be empty');
    finally
      ObjectId.Free;
    end;
  finally
    VS.Free;
  end;
end;

procedure TTestBoldFreeStandingValueSpace.TestValueSpaceGetHasContentsForId;
var
  VS: TBoldFreeStandingValueSpace;
  ObjectId1, ObjectId2: TBoldObjectId;
begin
  VS := TBoldFreeStandingValueSpace.Create;
  try
    ObjectId1 := TBoldInternalObjectId.CreateWithClassID(0, True);
    ObjectId2 := TBoldInternalObjectId.CreateWithClassID(1, True);
    try
      Assert.IsFalse(VS.GetHasContentsForId(ObjectId1), 'Should not have contents before ensure');
      VS.EnsureObjectContents(ObjectId1);
      Assert.IsTrue(VS.GetHasContentsForId(ObjectId1), 'Should have contents after ensure');
      Assert.IsFalse(VS.GetHasContentsForId(ObjectId2), 'Should not have contents for different ID');
    finally
      ObjectId1.Free;
      ObjectId2.Free;
    end;
  finally
    VS.Free;
  end;
end;

procedure TTestBoldFreeStandingValueSpace.TestValueSpaceGetFSObjectContentsByObjectId;
var
  VS: TBoldFreeStandingValueSpace;
  ObjectId: TBoldObjectId;
  OC: TBoldFreeStandingObjectContents;
begin
  VS := TBoldFreeStandingValueSpace.Create;
  try
    ObjectId := TBoldInternalObjectId.CreateWithClassID(0, True);
    try
      Assert.IsNull(VS.GetFSObjectContentsByObjectId(ObjectId), 'Should be nil before ensure');
      VS.EnsureObjectContents(ObjectId);
      OC := VS.GetFSObjectContentsByObjectId(ObjectId);
      Assert.IsNotNull(OC, 'Should return object contents after ensure');
    finally
      ObjectId.Free;
    end;
  finally
    VS.Free;
  end;
end;

procedure TTestBoldFreeStandingValueSpace.TestValueSpaceRemoveFSObjectContents;
var
  VS: TBoldFreeStandingValueSpace;
  ObjectId: TBoldObjectId;
begin
  VS := TBoldFreeStandingValueSpace.Create;
  try
    ObjectId := TBoldInternalObjectId.CreateWithClassID(0, True);
    try
      VS.EnsureObjectContents(ObjectId);
      Assert.AreEqual(1, VS.IdCount, 'IdCount should be 1');
      VS.RemoveFSObjectContentsByObjectId(ObjectId);
      // Note: RemoveFSObjectContents only removes from ObjectContentsList,
      // not from IdList. GetHasContentsForId checks ObjectContentsList.
      Assert.IsFalse(VS.GetHasContentsForId(ObjectId), 'Should not have contents after remove');
    finally
      ObjectId.Free;
    end;
  finally
    VS.Free;
  end;
end;

procedure TTestBoldFreeStandingValueSpace.TestValueSpaceAllObjectIds;
var
  VS: TBoldFreeStandingValueSpace;
  ObjectId1, ObjectId2: TBoldObjectId;
  ResultList: TBoldObjectIdList;
begin
  VS := TBoldFreeStandingValueSpace.Create;
  ResultList := TBoldObjectIdList.Create;
  try
    ObjectId1 := TBoldInternalObjectId.CreateWithClassID(0, True);
    ObjectId2 := TBoldInternalObjectId.CreateWithClassID(1, True);
    try
      VS.EnsureObjectContents(ObjectId1);
      VS.EnsureObjectContents(ObjectId2);
      VS.AllObjectIds(ResultList, False);
      Assert.AreEqual(2, ResultList.Count, 'Should have 2 object IDs');
    finally
      ObjectId1.Free;
      ObjectId2.Free;
    end;
  finally
    VS.Free;
    ResultList.Free;
  end;
end;

procedure TTestBoldFreeStandingValueSpace.TestValueSpaceClear;
var
  VS: TBoldFreeStandingValueSpace;
  ObjectId: TBoldObjectId;
begin
  VS := TBoldFreeStandingValueSpace.Create;
  try
    ObjectId := TBoldInternalObjectId.CreateWithClassID(0, True);
    try
      VS.EnsureObjectContents(ObjectId);
      Assert.AreEqual(1, VS.IdCount, 'IdCount should be 1');
      VS.Clear;
      Assert.AreEqual(0, VS.IdCount, 'IdCount should be 0 after clear');
      Assert.IsTrue(VS.IsEmpty, 'Should be empty after clear');
    finally
      ObjectId.Free;
    end;
  finally
    VS.Free;
  end;
end;

procedure TTestBoldFreeStandingValueSpace.TestValueSpaceGetAnyObjectId;
var
  VS: TBoldFreeStandingValueSpace;
  ObjectId: TBoldObjectId;
  AnyId: TBoldObjectId;
begin
  VS := TBoldFreeStandingValueSpace.Create;
  try
    Assert.IsNull(VS.GetAnyObjectId, 'GetAnyObjectId should be nil when empty');
    ObjectId := TBoldInternalObjectId.CreateWithClassID(0, True);
    try
      VS.EnsureObjectContents(ObjectId);
      AnyId := VS.GetAnyObjectId;
      Assert.IsNotNull(AnyId, 'GetAnyObjectId should return an ID');
    finally
      ObjectId.Free;
    end;
  finally
    VS.Free;
  end;
end;

procedure TTestBoldFreeStandingValueSpace.TestValueSpaceContentType;
begin
  Assert.AreEqual(Ord(bctValueSpace), Ord(TBoldFreeStandingValueSpace.ContentType),
    'ContentType should be bctValueSpace');
end;

// TBoldFreeStandingObjectContents tests

procedure TTestBoldFreeStandingValueSpace.TestObjectContentsCreate;
var
  OC: TBoldFreeStandingObjectContents;
begin
  OC := TBoldFreeStandingObjectContents.Create;
  try
    Assert.IsNotNull(OC, 'ObjectContents should be created');
  finally
    OC.Free;
  end;
end;

procedure TTestBoldFreeStandingValueSpace.TestObjectContentsIsEmptyInitially;
var
  OC: TBoldFreeStandingObjectContents;
begin
  OC := TBoldFreeStandingObjectContents.Create;
  try
    Assert.IsTrue(OC.IsEmpty, 'ObjectContents should be empty initially');
  finally
    OC.Free;
  end;
end;

procedure TTestBoldFreeStandingValueSpace.TestObjectContentsMemberCountZero;
var
  OC: TBoldFreeStandingObjectContents;
begin
  OC := TBoldFreeStandingObjectContents.Create;
  try
    Assert.AreEqual(0, OC.MemberCount, 'MemberCount should be 0 initially');
  finally
    OC.Free;
  end;
end;

procedure TTestBoldFreeStandingValueSpace.TestObjectContentsEnsureMemberAndGetValueByIndex;
var
  OC: TBoldFreeStandingObjectContents;
  Value: IBoldValue;
begin
  OC := TBoldFreeStandingObjectContents.Create;
  try
    Value := OC.EnsureMemberAndGetValueByIndex(0, 'String');
    Assert.IsNotNull(Value, 'EnsureMemberAndGetValueByIndex should create value');
    Assert.AreEqual(1, OC.MemberCount, 'MemberCount should be 1');
  finally
    FreeAfterTest(OC);
  end;
end;

procedure TTestBoldFreeStandingValueSpace.TestObjectContentsGetValueByIndex;
var
  OC: TBoldFreeStandingObjectContents;
begin
  OC := TBoldFreeStandingObjectContents.Create;
  try
    Assert.IsNull(OC.ValueByIndex[0], 'ValueByIndex should be nil for non-existent index');
    OC.EnsureMemberAndGetValueByIndex(0, 'String');
    Assert.IsNotNull(OC.ValueByIndex[0], 'ValueByIndex should return value after ensure');
  finally
    FreeAfterTest(OC);
  end;
end;

procedure TTestBoldFreeStandingValueSpace.TestObjectContentsExistenceState;
var
  OC: TBoldFreeStandingObjectContents;
begin
  OC := TBoldFreeStandingObjectContents.Create;
  try
    Assert.AreEqual(Ord(besExisting), Ord(OC.BoldExistenceState), 'Default should be besExisting');
    OC.BoldExistenceState := besNotCreated;
    Assert.AreEqual(Ord(besNotCreated), Ord(OC.BoldExistenceState), 'Should be besNotCreated');
    OC.BoldExistenceState := besDeleted;
    Assert.AreEqual(Ord(besDeleted), Ord(OC.BoldExistenceState), 'Should be besDeleted');
  finally
    OC.Free;
  end;
end;

procedure TTestBoldFreeStandingValueSpace.TestObjectContentsPersistenceState;
var
  OC: TBoldFreeStandingObjectContents;
begin
  OC := TBoldFreeStandingObjectContents.Create;
  try
    OC.BoldPersistenceState := bvpsCurrent;
    Assert.AreEqual(Ord(bvpsCurrent), Ord(OC.BoldPersistenceState), 'Should be bvpsCurrent');
    OC.BoldPersistenceState := bvpsModified;
    Assert.AreEqual(Ord(bvpsModified), Ord(OC.BoldPersistenceState), 'Should be bvpsModified');
  finally
    OC.Free;
  end;
end;

procedure TTestBoldFreeStandingValueSpace.TestObjectContentsTimeStamp;
var
  OC: TBoldFreeStandingObjectContents;
begin
  OC := TBoldFreeStandingObjectContents.Create;
  try
    Assert.AreEqual(-1, OC.TimeStamp, 'Default TimeStamp should be -1');
    OC.TimeStamp := 12345;
    Assert.AreEqual(12345, OC.TimeStamp, 'TimeStamp should be 12345');
  finally
    OC.Free;
  end;
end;

procedure TTestBoldFreeStandingValueSpace.TestObjectContentsMarkAllMembersCurrent;
var
  OC: TBoldFreeStandingObjectContents;
  Value: IBoldValue;
begin
  OC := TBoldFreeStandingObjectContents.Create;
  try
    Value := OC.EnsureMemberAndGetValueByIndex(0, 'String');
    Value.BoldPersistenceState := bvpsModified;
    OC.MarkAllMembersCurrent;
    Assert.AreEqual(Ord(bvpsCurrent), Ord(Value.BoldPersistenceState), 'Should be current after MarkAllMembersCurrent');
  finally
    FreeAfterTest(OC);
  end;
end;

function TTestBoldFreeStandingValueSpace.HasValueAt(OC: TBoldFreeStandingObjectContents;
  Index: Integer): Boolean;
begin
  Result := Assigned(OC.ValueByIndex[Index]);
end;

procedure TTestBoldFreeStandingValueSpace.TestObjectContentsRemoveMemberByIndex;
var
  OC: TBoldFreeStandingObjectContents;
begin
  OC := TBoldFreeStandingObjectContents.Create;
  try
    OC.EnsureMemberAndGetValueByIndex(0, 'String');
    // Checked through HasValueAt: its interface temporary is released before
    // RemoveMemberByIndex frees the value.
    Assert.IsTrue(HasValueAt(OC, 0), 'Value should exist');
    OC.RemoveMemberByIndex(0);
    Assert.IsFalse(HasValueAt(OC, 0), 'Value should be nil after remove');
  finally
    FreeAfterTest(OC);
  end;
end;

{ TTestBoldFreeStandingNullableValues }

procedure TTestBoldFreeStandingNullableValues.TestIntegerCreate;
var
  V: TBFSInteger;
begin
  V := TBFSInteger.Create;
  try
    Assert.IsNotNull(V, 'TBFSInteger should be created');
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestIntegerSetAndGet;
var
  V: TBFSInteger;
begin
  V := TBFSInteger.Create;
  try
    V.AsInteger := 42;
    Assert.AreEqual(42, V.AsInteger, 'AsInteger should be 42');
    V.AsInteger := -100;
    Assert.AreEqual(-100, V.AsInteger, 'AsInteger should be -100');
    V.AsInteger := MaxInt;
    Assert.AreEqual(MaxInt, V.AsInteger, 'AsInteger should be MaxInt');
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestIntegerIsNullInitially;
var
  V: TBFSInteger;
  Nullable: IBoldNullableValue;
begin
  V := TBFSInteger.Create;
  try
    Nullable := V as IBoldNullableValue;
    // Note: Default is NOT null - fIsNull defaults to False (Delphi boolean default)
    Assert.IsFalse(Nullable.IsNull, 'Default is not null (fIsNull defaults to False)');
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestIntegerSetToNonNull;
var
  V: TBFSInteger;
  Nullable: IBoldNullableValue;
begin
  V := TBFSInteger.Create;
  try
    Nullable := V as IBoldNullableValue;
    // Set to null explicitly first
    Nullable.SetContentToNull;
    Assert.IsTrue(Nullable.IsNull, 'Should be null after SetToNull');
    V.AsInteger := 0;
    Assert.IsFalse(Nullable.IsNull, 'Should not be null after setting value');
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestIntegerContentType;
begin
  Assert.AreEqual(Ord(bctInteger), Ord(TBFSInteger.ContentType), 'ContentType should be bctInteger');
end;

procedure TTestBoldFreeStandingNullableValues.TestIntegerIsEqualToValue;
var
  V1, V2: TBFSInteger;
begin
  V1 := TBFSInteger.Create;
  V2 := TBFSInteger.Create;
  try
    // Both null - should be equal
    Assert.IsTrue(V1.IsEqualToValue(V2 as IBoldValue), 'Two null integers should be equal');
    V1.AsInteger := 42;
    V2.AsInteger := 42;
    Assert.IsTrue(V1.IsEqualToValue(V2 as IBoldValue), 'Same value integers should be equal');
    V2.AsInteger := 43;
    Assert.IsFalse(V1.IsEqualToValue(V2 as IBoldValue), 'Different value integers should not be equal');
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestIntegerAssignContent;
var
  V1, V2: TBFSInteger;
begin
  V1 := TBFSInteger.Create;
  V2 := TBFSInteger.Create;
  try
    V1.AsInteger := 99;
    V2.AssignContent(V1 as IBoldValue);
    Assert.AreEqual(99, V2.AsInteger, 'Assigned value should be 99');
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestStringCreate;
var
  V: TBFSString;
begin
  V := TBFSString.Create;
  try
    Assert.IsNotNull(V, 'TBFSString should be created');
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestStringSetAndGet;
var
  V: TBFSString;
begin
  V := TBFSString.Create;
  try
    V.AsString := 'Hello';
    Assert.AreEqual('Hello', V.AsString, 'AsString should be Hello');
    V.AsString := '';
    Assert.AreEqual('', V.AsString, 'AsString should be empty');
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestStringIsNullInitially;
var
  V: TBFSString;
  Nullable: IBoldNullableValue;
begin
  V := TBFSString.Create;
  try
    Nullable := V as IBoldNullableValue;
    // Note: Default is NOT null - fIsNull defaults to False (Delphi boolean default)
    Assert.IsFalse(Nullable.IsNull, 'Default is not null (fIsNull defaults to False)');
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestStringContentType;
begin
  Assert.AreEqual(Ord(bctString), Ord(TBFSString.ContentType), 'ContentType should be bctString');
end;

procedure TTestBoldFreeStandingNullableValues.TestStringIsEqualToValue;
var
  V1, V2: TBFSString;
begin
  V1 := TBFSString.Create;
  V2 := TBFSString.Create;
  try
    V1.AsString := 'Test';
    V2.AsString := 'Test';
    Assert.IsTrue(V1.IsEqualToValue(V2 as IBoldValue), 'Same strings should be equal');
    V2.AsString := 'Different';
    Assert.IsFalse(V1.IsEqualToValue(V2 as IBoldValue), 'Different strings should not be equal');
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestStringAssignContent;
var
  V1, V2: TBFSString;
begin
  V1 := TBFSString.Create;
  V2 := TBFSString.Create;
  try
    V1.AsString := 'Copied';
    V2.AssignContent(V1 as IBoldValue);
    Assert.AreEqual('Copied', V2.AsString, 'Assigned value should be Copied');
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestFloatCreate;
var
  V: TBFSFloat;
begin
  V := TBFSFloat.Create;
  try
    Assert.IsNotNull(V, 'TBFSFloat should be created');
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestFloatSetAndGet;
var
  V: TBFSFloat;
begin
  V := TBFSFloat.Create;
  try
    V.AsFloat := 3.14159;
    Assert.AreEqual(3.14159, V.AsFloat, 0.00001, 'AsFloat should be 3.14159');
    V.AsFloat := -273.15;
    Assert.AreEqual(-273.15, V.AsFloat, 0.01, 'AsFloat should be -273.15');
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestFloatContentType;
begin
  Assert.AreEqual(Ord(bctFloat), Ord(TBFSFloat.ContentType), 'ContentType should be bctFloat');
end;

procedure TTestBoldFreeStandingNullableValues.TestFloatIsEqualToValue;
var
  V1, V2: TBFSFloat;
begin
  V1 := TBFSFloat.Create;
  V2 := TBFSFloat.Create;
  try
    V1.AsFloat := 1.5;
    V2.AsFloat := 1.5;
    Assert.IsTrue(V1.IsEqualToValue(V2 as IBoldValue), 'Same floats should be equal');
    V2.AsFloat := 1.6;
    Assert.IsFalse(V1.IsEqualToValue(V2 as IBoldValue), 'Different floats should not be equal');
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestCurrencyCreate;
var
  V: TBFSCurrency;
begin
  V := TBFSCurrency.Create;
  try
    Assert.IsNotNull(V, 'TBFSCurrency should be created');
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestCurrencySetAndGet;
var
  V: TBFSCurrency;
begin
  V := TBFSCurrency.Create;
  try
    V.AsCurrency := 123.45;
    Assert.AreEqual(123.45, V.AsCurrency, 0.001, 'AsCurrency should be 123.45');
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestCurrencyContentType;
begin
  Assert.AreEqual(Ord(bctCurrency), Ord(TBFSCurrency.ContentType), 'ContentType should be bctCurrency');
end;

procedure TTestBoldFreeStandingNullableValues.TestBooleanCreate;
var
  V: TBFSBoolean;
begin
  V := TBFSBoolean.Create;
  try
    Assert.IsNotNull(V, 'TBFSBoolean should be created');
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestBooleanSetAndGet;
var
  V: TBFSBoolean;
begin
  V := TBFSBoolean.Create;
  try
    V.AsBoolean := True;
    Assert.IsTrue(V.AsBoolean, 'AsBoolean should be True');
    V.AsBoolean := False;
    Assert.IsFalse(V.AsBoolean, 'AsBoolean should be False');
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestBooleanContentType;
begin
  Assert.AreEqual(Ord(bctBoolean), Ord(TBFSBoolean.ContentType), 'ContentType should be bctBoolean');
end;

procedure TTestBoldFreeStandingNullableValues.TestDateTimeCreate;
var
  V: TBFSDateTime;
begin
  V := TBFSDateTime.Create;
  try
    Assert.IsNotNull(V, 'TBFSDateTime should be created');
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestDateTimeSetAndGet;
var
  V: TBFSDateTime;
  DT: TDateTime;
begin
  V := TBFSDateTime.Create;
  try
    DT := Now;
    V.AsDateTime := DT;
    Assert.AreEqual(DT, V.AsDateTime, 'AsDateTime should match');
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestDateTimeContentType;
begin
  Assert.AreEqual(Ord(bctDateTime), Ord(TBFSDateTime.ContentType), 'ContentType should be bctDateTime');
end;

procedure TTestBoldFreeStandingNullableValues.TestDateCreate;
var
  V: TBFSDate;
begin
  V := TBFSDate.Create;
  try
    Assert.IsNotNull(V, 'TBFSDate should be created');
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestDateSetAndGet;
var
  V: TBFSDate;
  D: TDateTime;
begin
  V := TBFSDate.Create;
  try
    D := Date;
    V.AsDate := D;
    Assert.AreEqual(D, V.AsDate, 'AsDate should match');
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestDateContentType;
begin
  Assert.AreEqual(Ord(bctDate), Ord(TBFSDate.ContentType), 'ContentType should be bctDate');
end;

procedure TTestBoldFreeStandingNullableValues.TestTimeCreate;
var
  V: TBFSTime;
begin
  V := TBFSTime.Create;
  try
    Assert.IsNotNull(V, 'TBFSTime should be created');
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestTimeSetAndGet;
var
  V: TBFSTime;
  T: TDateTime;
begin
  V := TBFSTime.Create;
  try
    T := Time;
    V.AsTime := T;
    // Time comparison with small tolerance due to fractional seconds
    Assert.AreEqual(T, V.AsTime, 1/86400, 'AsTime should match within 1 second');
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestTimeContentType;
begin
  Assert.AreEqual(Ord(bctTime), Ord(TBFSTime.ContentType), 'ContentType should be bctTime');
end;

procedure TTestBoldFreeStandingNullableValues.TestBlobCreate;
var
  V: TBFSBlob;
begin
  V := TBFSBlob.Create;
  try
    Assert.IsNotNull(V, 'TBFSBlob should be created');
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestBlobSetAndGet;
var
  V: TBFSBlob;
  Data: AnsiString;
begin
  V := TBFSBlob.Create;
  try
    Data := 'BinaryData123';
    V.AsBlob := Data;
    Assert.AreEqual(string(Data), string(V.AsBlob), 'AsBlob should match');
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestBlobContentType;
begin
  Assert.AreEqual(Ord(bctBlob), Ord(TBFSBlob.ContentType), 'ContentType should be bctBlob');
end;

procedure TTestBoldFreeStandingNullableValues.TestTypedBlobCreate;
var
  V: TBFSTypedBlob;
begin
  V := TBFSTypedBlob.Create;
  try
    Assert.IsNotNull(V, 'TBFSTypedBlob should be created');
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestTypedBlobContentTypeProperty;
var
  V: TBFSTypedBlob;
begin
  V := TBFSTypedBlob.Create;
  try
    V.ContentTypeContent := 'image/png';
    Assert.AreEqual('image/png', V.ContentTypeContent, 'ContentTypeContent should be image/png');
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestTypedBlobContentType;
begin
  Assert.AreEqual(Ord(bctTypedBlob), Ord(TBFSTypedBlob.ContentType), 'ContentType should be bctTypedBlob');
end;

// TBFSAnsiString tests

procedure TTestBoldFreeStandingNullableValues.TestAnsiStringSetAndGet;
var
  V: TBFSAnsiString;
begin
  V := TBFSAnsiString.Create;
  try
    V.AsAnsiString := AnsiString('Hello');
    Assert.AreEqual(string(AnsiString('Hello')), string(V.AsAnsiString));
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestAnsiStringContentType;
begin
  Assert.AreEqual(Ord(bctAnsiString), Ord(TBFSAnsiString.ContentType));
end;

procedure TTestBoldFreeStandingNullableValues.TestAnsiStringAssignContent;
var
  V1, V2: TBFSAnsiString;
begin
  V1 := TBFSAnsiString.Create;
  V2 := TBFSAnsiString.Create;
  try
    V1.AsAnsiString := AnsiString('Test');
    V2.AssignContent(V1 as IBoldValue);
    Assert.AreEqual(string(AnsiString('Test')), string(V2.AsAnsiString));
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestAnsiStringIsEqualToValue;
var
  V1, V2: TBFSAnsiString;
begin
  V1 := TBFSAnsiString.Create;
  V2 := TBFSAnsiString.Create;
  try
    V1.AsAnsiString := AnsiString('Same');
    V2.AsAnsiString := AnsiString('Same');
    Assert.IsTrue(V1.IsEqualToValue(V2 as IBoldValue));
    V2.AsAnsiString := AnsiString('Different');
    Assert.IsFalse(V1.IsEqualToValue(V2 as IBoldValue));
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestAnsiStringGetContentAsString;
var
  V: TBFSAnsiString;
  StrRep: IBoldStringRepresentable;
begin
  V := TBFSAnsiString.Create;
  try
    V.AsAnsiString := AnsiString('TestStr');
    StrRep := V as IBoldStringRepresentable;
    Assert.AreEqual('TestStr', StrRep.AsString);
  finally
    FreeAfterTest(V);
  end;
end;

// TBFSUnicodeString tests

procedure TTestBoldFreeStandingNullableValues.TestUnicodeStringSetAndGet;
var
  V: TBFSUnicodeString;
begin
  V := TBFSUnicodeString.Create;
  try
    V.AsUnicodeString := 'Hello Unicode';
    Assert.AreEqual('Hello Unicode', string(V.AsUnicodeString));
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestUnicodeStringContentType;
begin
  Assert.AreEqual(Ord(bctUnicodeString), Ord(TBFSUnicodeString.ContentType));
end;

procedure TTestBoldFreeStandingNullableValues.TestUnicodeStringAssignContent;
var
  V1, V2: TBFSUnicodeString;
begin
  V1 := TBFSUnicodeString.Create;
  V2 := TBFSUnicodeString.Create;
  try
    V1.AsUnicodeString := 'UniTest';
    V2.AssignContent(V1 as IBoldValue);
    Assert.AreEqual('UniTest', string(V2.AsUnicodeString));
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestUnicodeStringIsEqualToValue;
var
  V1, V2: TBFSUnicodeString;
begin
  V1 := TBFSUnicodeString.Create;
  V2 := TBFSUnicodeString.Create;
  try
    V1.AsUnicodeString := 'Same';
    V2.AsUnicodeString := 'Same';
    Assert.IsTrue(V1.IsEqualToValue(V2 as IBoldValue));
    V2.AsUnicodeString := 'Different';
    Assert.IsFalse(V1.IsEqualToValue(V2 as IBoldValue));
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

// AssignContent for remaining types

procedure TTestBoldFreeStandingNullableValues.TestCurrencyAssignContent;
var
  V1, V2: TBFSCurrency;
begin
  V1 := TBFSCurrency.Create;
  V2 := TBFSCurrency.Create;
  try
    V1.AsCurrency := 99.99;
    V2.AssignContent(V1 as IBoldValue);
    Assert.AreEqual(Double(99.99), V2.AsCurrency, 0.001);
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestCurrencyIsEqualToValue;
var
  V1, V2: TBFSCurrency;
begin
  V1 := TBFSCurrency.Create;
  V2 := TBFSCurrency.Create;
  try
    V1.AsCurrency := 50.0;
    V2.AsCurrency := 50.0;
    Assert.IsTrue(V1.IsEqualToValue(V2 as IBoldValue));
    V2.AsCurrency := 50.01;
    Assert.IsFalse(V1.IsEqualToValue(V2 as IBoldValue));
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestFloatAssignContent;
var
  V1, V2: TBFSFloat;
begin
  V1 := TBFSFloat.Create;
  V2 := TBFSFloat.Create;
  try
    V1.AsFloat := 3.14;
    V2.AssignContent(V1 as IBoldValue);
    Assert.AreEqual(3.14, V2.AsFloat, 0.001);
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestBooleanAssignContent;
var
  V1, V2: TBFSBoolean;
begin
  V1 := TBFSBoolean.Create;
  V2 := TBFSBoolean.Create;
  try
    V1.AsBoolean := True;
    V2.AssignContent(V1 as IBoldValue);
    Assert.IsTrue(V2.AsBoolean);
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestBooleanIsEqualToValue;
var
  V1, V2: TBFSBoolean;
begin
  V1 := TBFSBoolean.Create;
  V2 := TBFSBoolean.Create;
  try
    V1.AsBoolean := True;
    V2.AsBoolean := True;
    Assert.IsTrue(V1.IsEqualToValue(V2 as IBoldValue));
    V2.AsBoolean := False;
    Assert.IsFalse(V1.IsEqualToValue(V2 as IBoldValue));
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestDateTimeAssignContent;
var
  V1, V2: TBFSDateTime;
  DT: TDateTime;
begin
  V1 := TBFSDateTime.Create;
  V2 := TBFSDateTime.Create;
  try
    DT := EncodeDate(2025, 6, 15) + EncodeTime(10, 30, 0, 0);
    V1.AsDateTime := DT;
    V2.AssignContent(V1 as IBoldValue);
    Assert.AreEqual(DT, V2.AsDateTime);
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestDateTimeIsEqualToValue;
var
  V1, V2: TBFSDateTime;
  DT: TDateTime;
begin
  V1 := TBFSDateTime.Create;
  V2 := TBFSDateTime.Create;
  try
    DT := EncodeDate(2025, 1, 1);
    V1.AsDateTime := DT;
    V2.AsDateTime := DT;
    Assert.IsTrue(V1.IsEqualToValue(V2 as IBoldValue));
    V2.AsDateTime := DT + 1;
    Assert.IsFalse(V1.IsEqualToValue(V2 as IBoldValue));
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestDateAssignContent;
var
  V1, V2: TBFSDate;
  D: TDateTime;
begin
  V1 := TBFSDate.Create;
  V2 := TBFSDate.Create;
  try
    D := EncodeDate(2025, 3, 20);
    V1.AsDate := D;
    V2.AssignContent(V1 as IBoldValue);
    Assert.AreEqual(D, V2.AsDate);
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestDateIsEqualToValue;
var
  V1, V2: TBFSDate;
  D: TDateTime;
begin
  V1 := TBFSDate.Create;
  V2 := TBFSDate.Create;
  try
    D := EncodeDate(2025, 3, 20);
    V1.AsDate := D;
    V2.AsDate := D;
    Assert.IsTrue(V1.IsEqualToValue(V2 as IBoldValue));
    V2.AsDate := D + 1;
    Assert.IsFalse(V1.IsEqualToValue(V2 as IBoldValue));
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestTimeAssignContent;
var
  V1, V2: TBFSTime;
  T: TDateTime;
begin
  V1 := TBFSTime.Create;
  V2 := TBFSTime.Create;
  try
    T := EncodeTime(14, 30, 0, 0);
    V1.AsTime := T;
    V2.AssignContent(V1 as IBoldValue);
    Assert.AreEqual(T, V2.AsTime);
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestTimeIsEqualToValue;
var
  V1, V2: TBFSTime;
  T: TDateTime;
begin
  V1 := TBFSTime.Create;
  V2 := TBFSTime.Create;
  try
    T := EncodeTime(10, 0, 0, 0);
    V1.AsTime := T;
    V2.AsTime := T;
    Assert.IsTrue(V1.IsEqualToValue(V2 as IBoldValue));
    V2.AsTime := EncodeTime(11, 0, 0, 0);
    Assert.IsFalse(V1.IsEqualToValue(V2 as IBoldValue));
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestBlobAssignContent;
var
  V1, V2: TBFSBlob;
begin
  V1 := TBFSBlob.Create;
  V2 := TBFSBlob.Create;
  try
    V1.AsBlob := AnsiString('BlobData');
    V2.AssignContent(V1 as IBoldValue);
    Assert.AreEqual(string(AnsiString('BlobData')), string(V2.AsBlob));
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestBlobIsEqualToValue;
var
  V1, V2: TBFSBlob;
begin
  V1 := TBFSBlob.Create;
  V2 := TBFSBlob.Create;
  try
    V1.AsBlob := AnsiString('Same');
    V2.AsBlob := AnsiString('Same');
    Assert.IsTrue(V1.IsEqualToValue(V2 as IBoldValue));
    V2.AsBlob := AnsiString('Different');
    Assert.IsFalse(V1.IsEqualToValue(V2 as IBoldValue));
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestTypedBlobAssignContent;
var
  V1, V2: TBFSTypedBlob;
begin
  V1 := TBFSTypedBlob.Create;
  V2 := TBFSTypedBlob.Create;
  try
    V1.AsBlob := AnsiString('BlobContent');
    V1.ContentTypeContent := 'text/plain';
    V2.AssignContent(V1 as IBoldValue);
    Assert.AreEqual(string(AnsiString('BlobContent')), string(V2.AsBlob));
    Assert.AreEqual('text/plain', V2.ContentTypeContent);
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestTypedBlobIsEqualToValue;
var
  V1, V2: TBFSTypedBlob;
begin
  V1 := TBFSTypedBlob.Create;
  V2 := TBFSTypedBlob.Create;
  try
    V1.AsBlob := AnsiString('Data');
    V1.ContentTypeContent := 'image/png';
    V2.AsBlob := AnsiString('Data');
    V2.ContentTypeContent := 'image/png';
    Assert.IsTrue(V1.IsEqualToValue(V2 as IBoldValue));
    V2.ContentTypeContent := 'text/html';
    Assert.IsFalse(V1.IsEqualToValue(V2 as IBoldValue));
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

// GetStringRepresentation / GetAsVariant

procedure TTestBoldFreeStandingNullableValues.TestNullableGetStringRepresentationNull;
var
  V: TBFSInteger;
  Nullable: IBoldNullableValue;
  StrRep: IBoldStringRepresentable;
begin
  V := TBFSInteger.Create;
  try
    Nullable := V as IBoldNullableValue;
    Nullable.SetContentToNull;
    StrRep := V as IBoldStringRepresentable;
    Assert.AreEqual('', StrRep.StringRepresentation[0]);
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestNullableGetStringRepresentationNonNull;
var
  V: TBFSInteger;
  StrRep: IBoldStringRepresentable;
begin
  V := TBFSInteger.Create;
  try
    V.AsInteger := 42;
    StrRep := V as IBoldStringRepresentable;
    Assert.AreEqual('42', StrRep.StringRepresentation[0]);
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestNullableGetAsVariantNull;
var
  V: TBFSInteger;
  VarReadable: IBoldVariantReadable;
begin
  V := TBFSInteger.Create;
  try
    (V as IBoldNullableValue).SetContentToNull;
    VarReadable := V as IBoldVariantReadable;
    Assert.IsTrue(VarIsNull(VarReadable.AsVariant));
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestNullableGetAsVariantNonNull;
var
  V: TBFSInteger;
  VarReadable: IBoldVariantReadable;
begin
  V := TBFSInteger.Create;
  try
    V.AsInteger := 99;
    VarReadable := V as IBoldVariantReadable;
    Assert.AreEqual(99, Integer(VarReadable.AsVariant));
  finally
    FreeAfterTest(V);
  end;
end;

{ TTestBoldFreeStandingIdRefs }

procedure TTestBoldFreeStandingIdRefs.TestObjectIdRefCreate;
var
  V: TBFSObjectIdRef;
begin
  V := TBFSObjectIdRef.Create;
  try
    Assert.IsNotNull(V, 'TBFSObjectIdRef should be created');
    Assert.IsNull(V.Id, 'Id should be nil initially');
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingIdRefs.TestObjectIdRefSetFromId;
var
  V: TBFSObjectIdRef;
  ObjId: TBoldObjectId;
begin
  V := TBFSObjectIdRef.Create;
  try
    ObjId := TBoldInternalObjectId.CreateWithClassID(5, True);
    try
      V.SetFromId(ObjId, False); // Don't adopt
      Assert.IsNotNull(V.Id, 'Id should be set');
      Assert.AreNotSame(ObjId, V.Id, 'Id should be cloned, not adopted');
      Assert.IsTrue(V.Id.IsEqual[ObjId], 'Id should equal original');
    finally
      ObjId.Free;
    end;
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingIdRefs.TestObjectIdRefSetFromIdAdopt;
var
  V: TBFSObjectIdRef;
  ObjId: TBoldObjectId;
begin
  V := TBFSObjectIdRef.Create;
  try
    ObjId := TBoldInternalObjectId.CreateWithClassID(5, True);
    V.SetFromId(ObjId, True); // Adopt - V now owns ObjId
    Assert.IsNotNull(V.Id, 'Id should be set');
    Assert.AreSame(ObjId, V.Id, 'Id should be adopted (same instance)');
    // Don't free ObjId - it's owned by V now
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingIdRefs.TestObjectIdRefContentType;
begin
  Assert.AreEqual(Ord(bctObjectIdRef), Ord(TBFSObjectIdRef.ContentType), 'ContentType should be bctObjectIdRef');
end;

procedure TTestBoldFreeStandingIdRefs.TestObjectIdRefGetStringRepresentation;
var
  V: TBFSObjectIdRef;
  ObjId: TBoldObjectId;
  StrRep: IBoldStringRepresentable;
begin
  V := TBFSObjectIdRef.Create;
  try
    ObjId := TBoldInternalObjectId.CreateWithClassID(10, True);
    try
      V.SetFromId(ObjId, False);
      StrRep := V as IBoldStringRepresentable;
      Assert.IsNotEmpty(StrRep.StringRepresentation[0], 'StringRepresentation should not be empty');
    finally
      ObjId.Free;
    end;
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingIdRefs.TestObjectIdRefIsEqualToValue;
var
  V1, V2: TBFSObjectIdRef;
  ObjId: TBoldObjectId;
begin
  V1 := TBFSObjectIdRef.Create;
  V2 := TBFSObjectIdRef.Create;
  try
    ObjId := TBoldInternalObjectId.CreateWithClassID(5, True);
    try
      V1.SetFromId(ObjId, False);
      V2.SetFromId(ObjId, False);
      Assert.IsTrue(V1.IsEqualToValue(V2 as IBoldValue), 'Same IDs should be equal');
    finally
      ObjId.Free;
    end;
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingIdRefs.TestObjectIdRefAssignContent;
var
  V1, V2: TBFSObjectIdRef;
  ObjId: TBoldObjectId;
begin
  V1 := TBFSObjectIdRef.Create;
  V2 := TBFSObjectIdRef.Create;
  try
    ObjId := TBoldInternalObjectId.CreateWithClassID(7, True);
    try
      V1.SetFromId(ObjId, False);
      V2.AssignContent(V1 as IBoldValue);
      Assert.IsNotNull(V2.Id, 'V2.Id should be set after assign');
      Assert.IsTrue(V2.Id.IsEqual[ObjId], 'V2.Id should equal original');
    finally
      ObjId.Free;
    end;
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingIdRefs.TestObjectIdListRefCreate;
var
  V: TBFSObjectIdListRef;
begin
  V := TBFSObjectIdListRef.Create;
  try
    Assert.IsNotNull(V, 'TBFSObjectIdListRef should be created');
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingIdRefs.TestObjectIdListRefCountZero;
var
  V: TBFSObjectIdListRef;
begin
  V := TBFSObjectIdListRef.Create;
  try
    Assert.AreEqual(0, V.Count, 'Count should be 0 initially');
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingIdRefs.TestObjectIdListRefSetFromIdList;
var
  V: TBFSObjectIdListRef;
  IdList: TBoldObjectIdList;
  ObjId1, ObjId2: TBoldObjectId;
begin
  V := TBFSObjectIdListRef.Create;
  IdList := TBoldObjectIdList.Create;
  try
    ObjId1 := TBoldInternalObjectId.CreateWithClassID(1, True);
    ObjId2 := TBoldInternalObjectId.CreateWithClassID(2, True);
    try
      IdList.Add(ObjId1);
      IdList.Add(ObjId2);
      V.SetFromIdList(IdList);
      Assert.AreEqual(2, V.Count, 'Count should be 2 after SetFromIdList');
    finally
      ObjId1.Free;
      ObjId2.Free;
    end;
  finally
    V.Free;
    IdList.Free;
  end;
end;

procedure TTestBoldFreeStandingIdRefs.TestObjectIdListRefContentType;
begin
  Assert.AreEqual(Ord(bctObjectIdListRef), Ord(TBFSObjectIdListRef.ContentType), 'ContentType should be bctObjectIdListRef');
end;

procedure TTestBoldFreeStandingIdRefs.TestObjectIdListRefAddAndRemoveId;
var
  V: TBFSObjectIdListRef;
  ObjId: TBoldObjectId;
  IdList: IBoldFreeStandingIdList;
begin
  V := TBFSObjectIdListRef.Create;
  try
    ObjId := TBoldInternalObjectId.CreateWithClassID(1, True);
    try
      IdList := V as IBoldFreeStandingIdList;
      IdList.AddId(ObjId);
      Assert.AreEqual(1, V.Count, 'Count should be 1 after AddId');
      IdList.RemoveId(ObjId);
      Assert.AreEqual(0, V.Count, 'Count should be 0 after RemoveId');
    finally
      ObjId.Free;
    end;
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingIdRefs.TestObjectIdListRefPairCreate;
var
  V: TBFSObjectIdListRefPair;
begin
  V := TBFSObjectIdListRefPair.Create;
  try
    Assert.IsNotNull(V, 'TBFSObjectIdListRefPair should be created');
  finally
    V.Free;
  end;
end;

procedure TTestBoldFreeStandingIdRefs.TestObjectIdListRefPairSetFromIdLists;
var
  V: TBFSObjectIdListRefPair;
  IdList1, IdList2: TBoldObjectIdList;
  ObjId1, ObjId2: TBoldObjectId;
begin
  V := TBFSObjectIdListRefPair.Create;
  IdList1 := TBoldObjectIdList.Create;
  IdList2 := TBoldObjectIdList.Create;
  try
    ObjId1 := TBoldInternalObjectId.CreateWithClassID(1, True);
    ObjId2 := TBoldInternalObjectId.CreateWithClassID(2, True);
    try
      IdList1.Add(ObjId1);
      IdList2.Add(ObjId2);
      V.SetFromIdLists(IdList1, IdList2);
      Assert.AreEqual(1, V.Count, 'Count should be 1 after SetFromIdLists');
    finally
      ObjId1.Free;
      ObjId2.Free;
    end;
  finally
    V.Free;
    IdList1.Free;
    IdList2.Free;
  end;
end;

procedure TTestBoldFreeStandingIdRefs.TestObjectIdListRefPairContentType;
begin
  Assert.AreEqual(Ord(bctObjectIdListRefPair), Ord(TBFSObjectIdListRefPair.ContentType), 'ContentType should be bctObjectIdListRefPair');
end;

procedure TTestBoldFreeStandingIdRefs.TestObjectIdListRefPairAddIds;
var
  V: TBFSObjectIdListRefPair;
  ObjId1, ObjId2: TBoldObjectId;
  IdListPair: IBoldFreeStandingIdListPair;
begin
  V := TBFSObjectIdListRefPair.Create;
  try
    ObjId1 := TBoldInternalObjectId.CreateWithClassID(1, True);
    ObjId2 := TBoldInternalObjectId.CreateWithClassID(2, True);
    try
      IdListPair := V as IBoldFreeStandingIdListPair;
      IdListPair.AddIds(ObjId1, ObjId2);
      Assert.AreEqual(1, V.Count, 'Count should be 1 after AddIds');
    finally
      ObjId1.Free;
      ObjId2.Free;
    end;
  finally
    FreeAfterTest(V);
  end;
end;

{ TTestableObjectIdRefPair }

procedure TTestableObjectIdRefPair.TestApplyTranslationList(TranslationList: TBoldIdTranslationList);
begin
  // Expose the protected method for testing
  ApplyTranslationList(TranslationList);
end;

{ TTestBoldFreeStandingValues }

procedure TTestBoldFreeStandingValues.TestObjectIdRefPairGetId1Empty;
var
  IdRefPair: TBFSObjectIdRefPair;
begin
  IdRefPair := TBFSObjectIdRefPair.Create;
  try
    // Empty pair should return nil for Id1
    Assert.IsNull(IdRefPair.Id1, 'Id1 should be nil when no IDs set');
  finally
    IdRefPair.Free;
  end;
end;

procedure TTestBoldFreeStandingValues.TestObjectIdRefPairGetId2Empty;
var
  IdRefPair: TBFSObjectIdRefPair;
begin
  IdRefPair := TBFSObjectIdRefPair.Create;
  try
    // Empty pair should return nil for Id2
    Assert.IsNull(IdRefPair.Id2, 'Id2 should be nil when no IDs set');
  finally
    IdRefPair.Free;
  end;
end;

procedure TTestBoldFreeStandingValues.TestObjectIdRefPairGetId1WithOneId;
var
  IdRefPair: TBFSObjectIdRefPair;
  Id1: TBoldInternalObjectId;
begin
  IdRefPair := TBFSObjectIdRefPair.Create;
  try
    Id1 := TBoldInternalObjectId.CreateWithClassId(1, False);
    try
      IdRefPair.SetFromIds(Id1, nil);
      Assert.IsNotNull(IdRefPair.Id1, 'Id1 should not be nil');
      Assert.IsNull(IdRefPair.Id2, 'Id2 should be nil when only Id1 set');
    finally
      Id1.Free;
    end;
  finally
    IdRefPair.Free;
  end;
end;

procedure TTestBoldFreeStandingValues.TestObjectIdRefPairGetId1And2WithTwoIds;
var
  IdRefPair: TBFSObjectIdRefPair;
  Id1, Id2: TBoldInternalObjectId;
begin
  IdRefPair := TBFSObjectIdRefPair.Create;
  try
    Id1 := TBoldInternalObjectId.CreateWithClassId(1, False);
    Id2 := TBoldInternalObjectId.CreateWithClassId(2, False);
    try
      IdRefPair.SetFromIds(Id1, Id2);
      Assert.IsNotNull(IdRefPair.Id1, 'Id1 should not be nil');
      Assert.IsNotNull(IdRefPair.Id2, 'Id2 should not be nil');
    finally
      Id1.Free;
      Id2.Free;
    end;
  finally
    IdRefPair.Free;
  end;
end;

procedure TTestBoldFreeStandingValues.TestSetFromIdsVerifyActualIdValues;
var
  IdRefPair: TBFSObjectIdRefPair;
  Id1, Id2: TBoldInternalObjectId;
begin
  IdRefPair := TBFSObjectIdRefPair.Create;
  try
    Id1 := TBoldInternalObjectId.CreateWithClassId(10, True);
    Id2 := TBoldInternalObjectId.CreateWithClassId(20, True);
    try
      IdRefPair.SetFromIds(Id1, Id2);
      Assert.IsTrue(IdRefPair.Id1.IsEqual[Id1], 'Id1 should equal the original Id1');
      Assert.IsTrue(IdRefPair.Id2.IsEqual[Id2], 'Id2 should equal the original Id2');
      Assert.AreEqual(10, IdRefPair.Id1.TopSortedIndex, 'Id1 TopSortedIndex should be 10');
      Assert.AreEqual(20, IdRefPair.Id2.TopSortedIndex, 'Id2 TopSortedIndex should be 20');
    finally
      Id1.Free;
      Id2.Free;
    end;
  finally
    IdRefPair.Free;
  end;
end;

procedure TTestBoldFreeStandingValues.TestSetFromIdsClonesIds;
var
  IdRefPair: TBFSObjectIdRefPair;
  Id1, Id2: TBoldInternalObjectId;
begin
  // Verify that SetFromIds clones the IDs (doesn't adopt them)
  IdRefPair := TBFSObjectIdRefPair.Create;
  try
    Id1 := TBoldInternalObjectId.CreateWithClassId(1, False);
    Id2 := TBoldInternalObjectId.CreateWithClassId(2, False);
    try
      IdRefPair.SetFromIds(Id1, Id2);
      // The IdRefPair should have cloned the IDs, not adopted them
      // So Id1 and Id2 should still be valid and different objects
      Assert.AreNotSame(Id1, IdRefPair.Id1, 'Id1 should be cloned, not the same instance');
      Assert.AreNotSame(Id2, IdRefPair.Id2, 'Id2 should be cloned, not the same instance');
    finally
      Id1.Free;
      Id2.Free;
    end;
  finally
    IdRefPair.Free;
  end;
end;

procedure TTestBoldFreeStandingValues.TestSetFromIdsReplacesExistingIds;
var
  IdRefPair: TBFSObjectIdRefPair;
  Id1A, Id2A, Id1B, Id2B: TBoldInternalObjectId;
begin
  IdRefPair := TBFSObjectIdRefPair.Create;
  try
    Id1A := TBoldInternalObjectId.CreateWithClassId(1, False);
    Id2A := TBoldInternalObjectId.CreateWithClassId(2, False);
    Id1B := TBoldInternalObjectId.CreateWithClassId(3, False);
    Id2B := TBoldInternalObjectId.CreateWithClassId(4, False);
    try
      // Set initial IDs
      IdRefPair.SetFromIds(Id1A, Id2A);
      Assert.IsTrue(IdRefPair.Id1.IsEqual[Id1A], 'Initial Id1 should be Id1A');
      Assert.IsTrue(IdRefPair.Id2.IsEqual[Id2A], 'Initial Id2 should be Id2A');

      // Replace with new IDs
      IdRefPair.SetFromIds(Id1B, Id2B);
      Assert.IsTrue(IdRefPair.Id1.IsEqual[Id1B], 'Replaced Id1 should be Id1B');
      Assert.IsTrue(IdRefPair.Id2.IsEqual[Id2B], 'Replaced Id2 should be Id2B');
    finally
      Id1A.Free;
      Id2A.Free;
      Id1B.Free;
      Id2B.Free;
    end;
  finally
    IdRefPair.Free;
  end;
end;

procedure TTestBoldFreeStandingValues.TestSetFromIdsWithBothNil;
var
  IdRefPair: TBFSObjectIdRefPair;
  Id1, Id2: TBoldInternalObjectId;
begin
  IdRefPair := TBFSObjectIdRefPair.Create;
  try
    // First set some IDs
    Id1 := TBoldInternalObjectId.CreateWithClassId(1, False);
    Id2 := TBoldInternalObjectId.CreateWithClassId(2, False);
    try
      IdRefPair.SetFromIds(Id1, Id2);
      Assert.IsNotNull(IdRefPair.Id1, 'Id1 should be set');
      Assert.IsNotNull(IdRefPair.Id2, 'Id2 should be set');
    finally
      Id1.Free;
      Id2.Free;
    end;

    // Now clear by setting both to nil
    IdRefPair.SetFromIds(nil, nil);
    Assert.IsNull(IdRefPair.Id1, 'Id1 should be nil after clearing');
    Assert.IsNull(IdRefPair.Id2, 'Id2 should be nil after clearing');
  finally
    IdRefPair.Free;
  end;
end;

procedure TTestBoldFreeStandingValues.TestOrderNoDefaultValue;
var
  IdRefPair: TBFSObjectIdRefPair;
begin
  IdRefPair := TBFSObjectIdRefPair.Create;
  try
    Assert.AreEqual(0, IdRefPair.OrderNo, 'OrderNo should default to 0');
  finally
    IdRefPair.Free;
  end;
end;

procedure TTestBoldFreeStandingValues.TestOrderNoSetAndGet;
var
  IdRefPair: TBFSObjectIdRefPair;
begin
  IdRefPair := TBFSObjectIdRefPair.Create;
  try
    IdRefPair.OrderNo := 42;
    Assert.AreEqual(42, IdRefPair.OrderNo, 'OrderNo should be 42');

    IdRefPair.OrderNo := -1;
    Assert.AreEqual(-1, IdRefPair.OrderNo, 'OrderNo should allow negative values');

    IdRefPair.OrderNo := MaxInt;
    Assert.AreEqual(MaxInt, IdRefPair.OrderNo, 'OrderNo should allow MaxInt');
  finally
    IdRefPair.Free;
  end;
end;

procedure TTestBoldFreeStandingValues.TestContentTypeReturnsObjectIdRefPair;
var
  IdRefPair: TBFSObjectIdRefPair;
begin
  IdRefPair := TBFSObjectIdRefPair.Create;
  try
    Assert.AreEqual(Ord(bctObjectIdRefPair), Ord(IdRefPair.ContentType),
      'ContentType should be bctObjectIdRefPair');
  finally
    IdRefPair.Free;
  end;
end;

procedure TTestBoldFreeStandingValues.TestGetStreamNameReturnsCorrectValue;
var
  IdRefPair: TBFSObjectIdRefPair;
  Value: IBoldValue;
begin
  IdRefPair := TBFSObjectIdRefPair.Create;
  try
    Value := IdRefPair as IBoldValue;
    Assert.AreEqual('ObjectIdRefPair', Value.ContentName,
      'StreamName should be "ObjectIdRefPair"');
  finally
    FreeAfterTest(IdRefPair);
  end;
end;

procedure TTestBoldFreeStandingValues.TestAssignContentValueCopiesIds;
var
  Source, Target: TBFSObjectIdRefPair;
  Id1, Id2: TBoldInternalObjectId;
begin
  Source := TBFSObjectIdRefPair.Create;
  Target := TBFSObjectIdRefPair.Create;
  try
    Id1 := TBoldInternalObjectId.CreateWithClassId(100, True);
    Id2 := TBoldInternalObjectId.CreateWithClassId(200, True);
    try
      Source.SetFromIds(Id1, Id2);
      Target.AssignContent(Source as IBoldValue);

      Assert.IsNotNull(Target.Id1, 'Target Id1 should not be nil');
      Assert.IsNotNull(Target.Id2, 'Target Id2 should not be nil');
      Assert.IsTrue(Target.Id1.IsEqual[Id1], 'Target Id1 should equal Source Id1');
      Assert.IsTrue(Target.Id2.IsEqual[Id2], 'Target Id2 should equal Source Id2');
    finally
      Id1.Free;
      Id2.Free;
    end;
  finally
    FreeAfterTest(Source);
    FreeAfterTest(Target);
  end;
end;

procedure TTestBoldFreeStandingValues.TestAssignContentValueCopiesOrderNo;
var
  Source, Target: TBFSObjectIdRefPair;
  Id1, Id2: TBoldInternalObjectId;
begin
  Source := TBFSObjectIdRefPair.Create;
  Target := TBFSObjectIdRefPair.Create;
  try
    Id1 := TBoldInternalObjectId.CreateWithClassId(1, False);
    Id2 := TBoldInternalObjectId.CreateWithClassId(2, False);
    try
      Source.SetFromIds(Id1, Id2);
      Source.OrderNo := 99;
      Target.AssignContent(Source as IBoldValue);

      Assert.AreEqual(99, Target.OrderNo, 'Target OrderNo should equal Source OrderNo');
    finally
      Id1.Free;
      Id2.Free;
    end;
  finally
    FreeAfterTest(Source);
    FreeAfterTest(Target);
  end;
end;

procedure TTestBoldFreeStandingValues.TestAssignContentValueWithNilIds;
var
  Source, Target: TBFSObjectIdRefPair;
begin
  Source := TBFSObjectIdRefPair.Create;
  Target := TBFSObjectIdRefPair.Create;
  try
    // Source has no IDs set (both nil)
    Target.AssignContent(Source as IBoldValue);

    Assert.IsNull(Target.Id1, 'Target Id1 should be nil when Source Id1 is nil');
    Assert.IsNull(Target.Id2, 'Target Id2 should be nil when Source Id2 is nil');
  finally
    FreeAfterTest(Source);
    FreeAfterTest(Target);
  end;
end;

procedure TTestBoldFreeStandingValues.TestApplyTranslationListTranslatesIds;
var
  IdRefPair: TTestableObjectIdRefPair;
  OldId1, OldId2, NewId1, NewId2: TBoldInternalObjectId;
  TranslationList: TBoldIdTranslationList;
begin
  IdRefPair := TTestableObjectIdRefPair.Create;
  TranslationList := TBoldIdTranslationList.Create;
  try
    OldId1 := TBoldInternalObjectId.CreateWithClassId(1, False);
    OldId2 := TBoldInternalObjectId.CreateWithClassId(2, False);
    NewId1 := TBoldInternalObjectId.CreateWithClassId(10, True);
    NewId2 := TBoldInternalObjectId.CreateWithClassId(20, True);
    try
      // Set up the pair with old IDs
      IdRefPair.SetFromIds(OldId1, OldId2);

      // Set up translation list
      TranslationList.AddTranslation(OldId1, NewId1);
      TranslationList.AddTranslation(OldId2, NewId2);

      // Apply translation
      IdRefPair.TestApplyTranslationList(TranslationList);

      // Verify IDs were translated
      Assert.IsTrue(IdRefPair.Id1.IsEqual[NewId1], 'Id1 should be translated to NewId1');
      Assert.IsTrue(IdRefPair.Id2.IsEqual[NewId2], 'Id2 should be translated to NewId2');
    finally
      OldId1.Free;
      OldId2.Free;
      NewId1.Free;
      NewId2.Free;
    end;
  finally
    IdRefPair.Free;
    TranslationList.Free;
  end;
end;

procedure TTestBoldFreeStandingValues.TestApplyTranslationListWithNoMatchingIds;
var
  IdRefPair: TTestableObjectIdRefPair;
  Id1, Id2, UnrelatedOldId, UnrelatedNewId: TBoldInternalObjectId;
  TranslationList: TBoldIdTranslationList;
begin
  IdRefPair := TTestableObjectIdRefPair.Create;
  TranslationList := TBoldIdTranslationList.Create;
  try
    Id1 := TBoldInternalObjectId.CreateWithClassId(1, False);
    Id2 := TBoldInternalObjectId.CreateWithClassId(2, False);
    UnrelatedOldId := TBoldInternalObjectId.CreateWithClassId(99, False);
    UnrelatedNewId := TBoldInternalObjectId.CreateWithClassId(100, False);
    try
      // Set up the pair
      IdRefPair.SetFromIds(Id1, Id2);

      // Translation list has unrelated IDs
      TranslationList.AddTranslation(UnrelatedOldId, UnrelatedNewId);

      // Apply translation - should not change anything
      IdRefPair.TestApplyTranslationList(TranslationList);

      // Verify IDs were NOT changed
      Assert.IsTrue(IdRefPair.Id1.IsEqual[Id1], 'Id1 should remain unchanged');
      Assert.IsTrue(IdRefPair.Id2.IsEqual[Id2], 'Id2 should remain unchanged');
    finally
      Id1.Free;
      Id2.Free;
      UnrelatedOldId.Free;
      UnrelatedNewId.Free;
    end;
  finally
    IdRefPair.Free;
    TranslationList.Free;
  end;
end;

procedure TTestBoldFreeStandingValues.TestApplyTranslationListWithNilIdList;
var
  IdRefPair: TTestableObjectIdRefPair;
  TranslationList: TBoldIdTranslationList;
begin
  // Test that ApplyTranslationList doesn't crash when internal fObjectIds is nil
  IdRefPair := TTestableObjectIdRefPair.Create;
  TranslationList := TBoldIdTranslationList.Create;
  try
    // IdRefPair has no IDs set (fObjectIds is nil)
    // This should not raise an exception
    IdRefPair.TestApplyTranslationList(TranslationList);

    // Verify still nil
    Assert.IsNull(IdRefPair.Id1, 'Id1 should remain nil');
    Assert.IsNull(IdRefPair.Id2, 'Id2 should remain nil');
  finally
    IdRefPair.Free;
    TranslationList.Free;
  end;
end;

procedure TTestBoldFreeStandingValues.TestGetStringRepresentationWithTwoIds;
var
  IdRefPair: TBFSObjectIdRefPair;
  Id1, Id2: TBoldInternalObjectId;
  StringRepresentable: IBoldStringRepresentable;
  S: string;
begin
  // This test demonstrates the bug: calling GetStringRepresentation on
  // TBFSObjectIdRefPair crashes with EAbstractError because the method
  // is not implemented (inherited abstract from TBFSObjectIDRefAbstract)
  IdRefPair := TBFSObjectIdRefPair.Create;
  try
    Id1 := TBoldInternalObjectId.CreateWithClassId(10, True);
    Id2 := TBoldInternalObjectId.CreateWithClassId(20, True);
    try
      IdRefPair.SetFromIds(Id1, Id2);

      // Get the interface - this should work
      Assert.IsTrue(Supports(IdRefPair, IBoldStringRepresentable, StringRepresentable),
        'TBFSObjectIdRefPair should support IBoldStringRepresentable');

      // Call GetStringRepresentation - this will crash with EAbstractError
      // before the fix is applied
      S := StringRepresentable.StringRepresentation[0];

      // Verify it contains both IDs
      Assert.Contains(S, Id1.AsString, 'StringRepresentation should contain Id1');
      Assert.Contains(S, Id2.AsString, 'StringRepresentation should contain Id2');
    finally
      Id1.Free;
      Id2.Free;
    end;
  finally
    FreeAfterTest(IdRefPair);
  end;
end;

procedure TTestBoldFreeStandingValues.TestGetStringRepresentationWithOneId;
var
  IdRefPair: TBFSObjectIdRefPair;
  Id1: TBoldInternalObjectId;
  StringRepresentable: IBoldStringRepresentable;
  S: string;
begin
  IdRefPair := TBFSObjectIdRefPair.Create;
  try
    Id1 := TBoldInternalObjectId.CreateWithClassId(10, True);
    try
      IdRefPair.SetFromIds(Id1, nil);

      StringRepresentable := IdRefPair as IBoldStringRepresentable;
      S := StringRepresentable.StringRepresentation[0];

      // Should contain Id1 and indicate nil for Id2
      Assert.Contains(S, Id1.AsString, 'StringRepresentation should contain Id1');
    finally
      Id1.Free;
    end;
  finally
    FreeAfterTest(IdRefPair);
  end;
end;

procedure TTestBoldFreeStandingValues.TestGetStringRepresentationEmpty;
var
  IdRefPair: TBFSObjectIdRefPair;
  StringRepresentable: IBoldStringRepresentable;
  S: string;
begin
  IdRefPair := TBFSObjectIdRefPair.Create;
  try
    // No IDs set
    StringRepresentable := IdRefPair as IBoldStringRepresentable;
    S := StringRepresentable.StringRepresentation[0];

    // Should not crash and should return some meaningful value
    Assert.IsNotNull(S, 'StringRepresentation should not be nil');
  finally
    FreeAfterTest(IdRefPair);
  end;
end;

procedure TTestBoldFreeStandingValues.TestGetContentAsString;
var
  IdRefPair: TBFSObjectIdRefPair;
  Id1, Id2: TBoldInternalObjectId;
  StringRepresentable: IBoldStringRepresentable;
  S: string;
begin
  IdRefPair := TBFSObjectIdRefPair.Create;
  try
    Id1 := TBoldInternalObjectId.CreateWithClassId(10, True);
    Id2 := TBoldInternalObjectId.CreateWithClassId(20, True);
    try
      IdRefPair.SetFromIds(Id1, Id2);

      StringRepresentable := IdRefPair as IBoldStringRepresentable;
      // GetContentAsString (asString property) should work
      S := StringRepresentable.asString;

      Assert.IsNotEmpty(S, 'asString should not be empty');
    finally
      Id1.Free;
      Id2.Free;
    end;
  finally
    FreeAfterTest(IdRefPair);
  end;
end;

procedure TTestBoldFreeStandingValues.TestIBoldStringRepresentableInterface;
var
  IdRefPair: TBFSObjectIdRefPair;
  Id1, Id2: TBoldInternalObjectId;
  Value: IBoldValue;
  StringRepresentable: IBoldStringRepresentable;
begin
  // This test verifies that TBFSObjectIdRefPair properly implements
  // IBoldStringRepresentable interface (required by BoldJSONWriter and others)
  IdRefPair := TBFSObjectIdRefPair.Create;
  try
    Id1 := TBoldInternalObjectId.CreateWithClassId(5, False);
    Id2 := TBoldInternalObjectId.CreateWithClassId(15, False);
    try
      IdRefPair.SetFromIds(Id1, Id2);

      Value := IdRefPair as IBoldValue;
      // QueryInterface for IBoldStringRepresentable - this is what BoldJSONWriter does
      Assert.AreEqual(S_OK, Value.QueryInterface(IBoldStringRepresentable, StringRepresentable),
        'QueryInterface for IBoldStringRepresentable should succeed');

      // This call crashes before the fix
      Assert.IsNotEmpty(StringRepresentable.StringRepresentation[0],
        'StringRepresentation should return non-empty string');
    finally
      Id1.Free;
      Id2.Free;
    end;
  finally
    FreeAfterTest(IdRefPair);
  end;
end;

// GetStreamName tests via IBoldStreamable

procedure TTestBoldFreeStandingNullableValues.TestIntegerGetStreamName;
var V: TBFSInteger;
begin
  V := TBFSInteger.Create;
  try
    Assert.AreEqual(BoldContentName_Integer, (V as IBoldStreamable).StreamName);
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestStringGetStreamName;
var V: TBFSString;
begin
  V := TBFSString.Create;
  try
    Assert.AreEqual(BoldContentName_String, (V as IBoldStreamable).StreamName);
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestAnsiStringGetStreamName;
var V: TBFSAnsiString;
begin
  V := TBFSAnsiString.Create;
  try
    Assert.AreEqual(BoldContentName_AnsiString, (V as IBoldStreamable).StreamName);
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestUnicodeStringGetStreamName;
var V: TBFSUnicodeString;
begin
  V := TBFSUnicodeString.Create;
  try
    Assert.AreEqual(BoldContentName_UnicodeString, (V as IBoldStreamable).StreamName);
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestCurrencyGetStreamName;
var V: TBFSCurrency;
begin
  V := TBFSCurrency.Create;
  try
    Assert.AreEqual(BoldContentName_Currency, (V as IBoldStreamable).StreamName);
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestFloatGetStreamName;
var V: TBFSFloat;
begin
  V := TBFSFloat.Create;
  try
    Assert.AreEqual(BoldContentName_Float, (V as IBoldStreamable).StreamName);
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestBooleanGetStreamName;
var V: TBFSBoolean;
begin
  V := TBFSBoolean.Create;
  try
    Assert.AreEqual(BoldContentName_Boolean, (V as IBoldStreamable).StreamName);
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestDateTimeGetStreamName;
var V: TBFSDateTime;
begin
  V := TBFSDateTime.Create;
  try
    Assert.AreEqual(BoldContentName_DateTime, (V as IBoldStreamable).StreamName);
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestDateGetStreamName;
var V: TBFSDate;
begin
  V := TBFSDate.Create;
  try
    Assert.AreEqual(BoldContentName_Date, (V as IBoldStreamable).StreamName);
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestTimeGetStreamName;
var V: TBFSTime;
begin
  V := TBFSTime.Create;
  try
    Assert.AreEqual(BoldContentName_Time, (V as IBoldStreamable).StreamName);
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestBlobGetStreamName;
var V: TBFSBlob;
begin
  V := TBFSBlob.Create;
  try
    Assert.AreEqual(BoldContentName_Blob, (V as IBoldStreamable).StreamName);
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestTypedBlobGetStreamName;
var V: TBFSTypedBlob;
begin
  V := TBFSTypedBlob.Create;
  try
    Assert.AreEqual(BoldContentName_TypedBlob, (V as IBoldStreamable).StreamName);
  finally
    FreeAfterTest(V);
  end;
end;

// GetValueAsVariant via IBoldVariantReadable

procedure TTestBoldFreeStandingNullableValues.TestStringGetValueAsVariant;
var V: TBFSString;
begin
  V := TBFSString.Create;
  try
    V.AsString := 'Test';
    Assert.AreEqual('Test', string((V as IBoldVariantReadable).AsVariant));
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestCurrencyGetValueAsVariant;
var V: TBFSCurrency;
begin
  V := TBFSCurrency.Create;
  try
    V.AsCurrency := 10.50;
    Assert.AreEqual(Double(10.50), Currency((V as IBoldVariantReadable).AsVariant), 0.01);
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestFloatGetValueAsVariant;
var V: TBFSFloat;
begin
  V := TBFSFloat.Create;
  try
    V.AsFloat := 2.5;
    Assert.AreEqual(2.5, Double((V as IBoldVariantReadable).AsVariant), 0.001);
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestBooleanGetValueAsVariant;
var V: TBFSBoolean;
begin
  V := TBFSBoolean.Create;
  try
    V.AsBoolean := True;
    Assert.IsTrue(Boolean((V as IBoldVariantReadable).AsVariant));
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestDateTimeGetValueAsVariant;
var
  V: TBFSDateTime;
  DT: TDateTime;
begin
  V := TBFSDateTime.Create;
  try
    DT := EncodeDate(2025, 1, 1);
    V.AsDateTime := DT;
    Assert.AreEqual(DT, TDateTime((V as IBoldVariantReadable).AsVariant));
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestDateGetValueAsVariant;
var
  V: TBFSDate;
  D: TDateTime;
begin
  V := TBFSDate.Create;
  try
    D := EncodeDate(2025, 6, 15);
    V.AsDate := D;
    Assert.AreEqual(D, TDateTime((V as IBoldVariantReadable).AsVariant));
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestTimeGetValueAsVariant;
var
  V: TBFSTime;
  T: TDateTime;
begin
  V := TBFSTime.Create;
  try
    T := EncodeTime(12, 0, 0, 0);
    V.AsTime := T;
    Assert.AreEqual(T, TDateTime((V as IBoldVariantReadable).AsVariant));
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestBlobGetValueAsVariant;
var V: TBFSBlob;
begin
  V := TBFSBlob.Create;
  try
    V.AsBlob := AnsiString('data');
    Assert.IsNotEmpty(string(AnsiString((V as IBoldVariantReadable).AsVariant)));
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestAnsiStringGetValueAsVariant;
var V: TBFSAnsiString;
begin
  V := TBFSAnsiString.Create;
  try
    V.AsAnsiString := AnsiString('test');
    Assert.IsNotEmpty(string((V as IBoldVariantReadable).AsVariant));
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestUnicodeStringGetValueAsVariant;
var V: TBFSUnicodeString;
begin
  V := TBFSUnicodeString.Create;
  try
    V.AsUnicodeString := 'test';
    Assert.AreEqual('test', string((V as IBoldVariantReadable).AsVariant));
  finally
    FreeAfterTest(V);
  end;
end;

// AssignContentValue null paths

procedure TTestBoldFreeStandingNullableValues.TestStringAssignContentNull;
var V1, V2: TBFSString;
begin
  V1 := TBFSString.Create;
  V2 := TBFSString.Create;
  try
    (V1 as IBoldNullableValue).SetContentToNull;
    V2.AssignContent(V1 as IBoldValue);
    Assert.IsTrue((V2 as IBoldNullableValue).IsNull);
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestAnsiStringAssignContentNull;
var V1, V2: TBFSAnsiString;
begin
  V1 := TBFSAnsiString.Create;
  V2 := TBFSAnsiString.Create;
  try
    (V1 as IBoldNullableValue).SetContentToNull;
    V2.AssignContent(V1 as IBoldValue);
    Assert.IsTrue((V2 as IBoldNullableValue).IsNull);
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestUnicodeStringAssignContentNull;
var V1, V2: TBFSUnicodeString;
begin
  V1 := TBFSUnicodeString.Create;
  V2 := TBFSUnicodeString.Create;
  try
    (V1 as IBoldNullableValue).SetContentToNull;
    V2.AssignContent(V1 as IBoldValue);
    Assert.IsTrue((V2 as IBoldNullableValue).IsNull);
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestIntegerAssignContentNull;
var V1, V2: TBFSInteger;
begin
  V1 := TBFSInteger.Create;
  V2 := TBFSInteger.Create;
  try
    (V1 as IBoldNullableValue).SetContentToNull;
    V2.AssignContent(V1 as IBoldValue);
    Assert.IsTrue((V2 as IBoldNullableValue).IsNull);
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestCurrencyAssignContentNull;
var V1, V2: TBFSCurrency;
begin
  V1 := TBFSCurrency.Create;
  V2 := TBFSCurrency.Create;
  try
    (V1 as IBoldNullableValue).SetContentToNull;
    V2.AssignContent(V1 as IBoldValue);
    Assert.IsTrue((V2 as IBoldNullableValue).IsNull);
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestFloatAssignContentNull;
var V1, V2: TBFSFloat;
begin
  V1 := TBFSFloat.Create;
  V2 := TBFSFloat.Create;
  try
    (V1 as IBoldNullableValue).SetContentToNull;
    V2.AssignContent(V1 as IBoldValue);
    Assert.IsTrue((V2 as IBoldNullableValue).IsNull);
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestBooleanAssignContentNull;
var V1, V2: TBFSBoolean;
begin
  V1 := TBFSBoolean.Create;
  V2 := TBFSBoolean.Create;
  try
    (V1 as IBoldNullableValue).SetContentToNull;
    V2.AssignContent(V1 as IBoldValue);
    Assert.IsTrue((V2 as IBoldNullableValue).IsNull);
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestDateTimeAssignContentNull;
var V1, V2: TBFSDateTime;
begin
  V1 := TBFSDateTime.Create;
  V2 := TBFSDateTime.Create;
  try
    (V1 as IBoldNullableValue).SetContentToNull;
    V2.AssignContent(V1 as IBoldValue);
    Assert.IsTrue((V2 as IBoldNullableValue).IsNull);
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestDateAssignContentNull;
var V1, V2: TBFSDate;
begin
  V1 := TBFSDate.Create;
  V2 := TBFSDate.Create;
  try
    (V1 as IBoldNullableValue).SetContentToNull;
    V2.AssignContent(V1 as IBoldValue);
    Assert.IsTrue((V2 as IBoldNullableValue).IsNull);
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestTimeAssignContentNull;
var V1, V2: TBFSTime;
begin
  V1 := TBFSTime.Create;
  V2 := TBFSTime.Create;
  try
    (V1 as IBoldNullableValue).SetContentToNull;
    V2.AssignContent(V1 as IBoldValue);
    Assert.IsTrue((V2 as IBoldNullableValue).IsNull);
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestBlobAssignContentNull;
var V1, V2: TBFSBlob;
begin
  V1 := TBFSBlob.Create;
  V2 := TBFSBlob.Create;
  try
    (V1 as IBoldNullableValue).SetContentToNull;
    V2.AssignContent(V1 as IBoldValue);
    Assert.IsTrue((V2 as IBoldNullableValue).IsNull);
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestTypedBlobAssignContentNull;
var V1, V2: TBFSTypedBlob;
begin
  V1 := TBFSTypedBlob.Create;
  V2 := TBFSTypedBlob.Create;
  try
    // Note: SetContentTypeContent calls SetToNonNull, so even when blob is null,
    // the ContentTypeContent assignment makes it non-null
    (V1 as IBoldNullableValue).SetContentToNull;
    V1.ContentTypeContent := 'text/plain';
    V2.AssignContent(V1 as IBoldValue);
    // After assign, ContentTypeContent is copied (which calls SetToNonNull)
    Assert.AreEqual('text/plain', V2.ContentTypeContent);
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFreeStandingNullableValues.TestNullableBaseGetContentAsString;
var
  V: TBFSInteger;
  StrRep: IBoldStringRepresentable;
begin
  V := TBFSInteger.Create;
  try
    V.AsInteger := 42;
    StrRep := V as IBoldStringRepresentable;
    Assert.AreEqual('42', StrRep.AsString);
  finally
    FreeAfterTest(V);
  end;
end;

{ TTestBoldFSValueSpaceExtended }

procedure TTestBoldFSValueSpaceExtended.TestRemoveDeletedObjects;
var
  VS: TBoldFreeStandingValueSpace;
  Id1, Id2: TBoldObjectId;
  OC: TBoldFreeStandingObjectContents;
begin
  VS := TBoldFreeStandingValueSpace.Create;
  try
    Id1 := TBoldInternalObjectId.CreateWithClassID(0, True);
    Id2 := TBoldInternalObjectId.CreateWithClassID(1, True);
    try
      VS.EnsureObjectContents(Id1);
      VS.EnsureObjectContents(Id2);
      OC := VS.GetFSObjectContentsByObjectId(Id1);
      OC.BoldExistenceState := besDeleted;
      VS.RemoveDeletedObjects;
      Assert.IsFalse(VS.GetHasContentsForId(Id1));
      Assert.IsTrue(VS.GetHasContentsForId(Id2));
    finally
      Id1.Free;
      Id2.Free;
    end;
  finally
    VS.Free;
  end;
end;

procedure TTestBoldFSValueSpaceExtended.TestMarkAllObjectsAndMembersCurrent;
var
  VS: TBoldFreeStandingValueSpace;
  Id: TBoldObjectId;
  OC: TBoldFreeStandingObjectContents;
  V: IBoldValue;
begin
  VS := TBoldFreeStandingValueSpace.Create;
  try
    Id := TBoldInternalObjectId.CreateWithClassID(0, True);
    try
      VS.EnsureObjectContents(Id);
      OC := VS.GetFSObjectContentsByObjectId(Id);
      V := OC.EnsureMemberAndGetValueByIndex(0, BoldContentName_String);
      V.BoldPersistenceState := bvpsModified;
      OC.BoldPersistenceState := bvpsModified;
      VS.MarkAllObjectsAndMembersCurrent;
      Assert.AreEqual(Ord(bvpsCurrent), Ord(OC.BoldPersistenceState));
      Assert.AreEqual(Ord(bvpsCurrent), Ord(V.BoldPersistenceState));
    finally
      Id.Free;
    end;
  finally
    FreeAfterTest(VS);
  end;
end;

procedure TTestBoldFSValueSpaceExtended.TestRemoveAllObjectContents;
var
  VS: TBoldFreeStandingValueSpace;
  Id: TBoldObjectId;
begin
  VS := TBoldFreeStandingValueSpace.Create;
  try
    Id := TBoldInternalObjectId.CreateWithClassID(0, True);
    try
      VS.EnsureObjectContents(Id);
      Assert.IsTrue(VS.GetHasContentsForId(Id));
      VS.RemoveAllObjectContents;
      Assert.IsFalse(VS.GetHasContentsForId(Id));
    finally
      Id.Free;
    end;
  finally
    VS.Free;
  end;
end;

procedure TTestBoldFSValueSpaceExtended.TestClearWhenObjectContentsEmpty;
var
  VS: TBoldFreeStandingValueSpace;
  Id: TBoldObjectId;
begin
  VS := TBoldFreeStandingValueSpace.Create;
  try
    Id := TBoldInternalObjectId.CreateWithClassID(0, True);
    try
      VS.EnsureObjectContents(Id);
      VS.RemoveAllObjectContents;
      // Now object contents list is empty but IdList still has entry
      Assert.AreEqual(1, VS.IdCount);
      VS.ClearWhenObjectContentsEmpty;
      Assert.AreEqual(0, VS.IdCount);
    finally
      Id.Free;
    end;
  finally
    VS.Free;
  end;
end;

procedure TTestBoldFSValueSpaceExtended.TestClearWhenObjectContentsNotEmpty;
var
  VS: TBoldFreeStandingValueSpace;
  Id: TBoldObjectId;
begin
  VS := TBoldFreeStandingValueSpace.Create;
  try
    Id := TBoldInternalObjectId.CreateWithClassID(0, True);
    try
      VS.EnsureObjectContents(Id);
      // Object contents exist - ClearWhenObjectContentsEmpty should not clear
      VS.ClearWhenObjectContentsEmpty;
      Assert.AreEqual(1, VS.IdCount);
    finally
      Id.Free;
    end;
  finally
    VS.Free;
  end;
end;

procedure TTestBoldFSValueSpaceExtended.TestGetValueForIdAndMemberIndex;
var
  VS: TBoldFreeStandingValueSpace;
  Id: TBoldObjectId;
  OC: TBoldFreeStandingObjectContents;
  V: IBoldValue;
begin
  VS := TBoldFreeStandingValueSpace.Create;
  try
    Id := TBoldInternalObjectId.CreateWithClassID(0, True);
    try
      VS.EnsureObjectContents(Id);
      OC := VS.GetFSObjectContentsByObjectId(Id);
      OC.EnsureMemberAndGetValueByIndex(0, BoldContentName_Integer);
      V := VS.GetValueForIdAndMemberIndex(Id, 0);
      Assert.IsNotNull(V);
    finally
      Id.Free;
    end;
  finally
    FreeAfterTest(VS);
  end;
end;

procedure TTestBoldFSValueSpaceExtended.TestGetValueForIdAndMemberIndexNotFound;
var
  VS: TBoldFreeStandingValueSpace;
  Id: TBoldObjectId;
  V: IBoldValue;
begin
  VS := TBoldFreeStandingValueSpace.Create;
  try
    Id := TBoldInternalObjectId.CreateWithClassID(99, True);
    try
      V := VS.GetValueForIdAndMemberIndex(Id, 0);
      Assert.IsNull(V);
    finally
      Id.Free;
    end;
  finally
    FreeAfterTest(VS);
  end;
end;

procedure TTestBoldFSValueSpaceExtended.TestAllObjectIdsOnlyLoaded;
var
  VS: TBoldFreeStandingValueSpace;
  Id1, Id2: TBoldObjectId;
  ResultList: TBoldObjectIdList;
begin
  VS := TBoldFreeStandingValueSpace.Create;
  ResultList := TBoldObjectIdList.Create;
  try
    Id1 := TBoldInternalObjectId.CreateWithClassID(0, True);
    Id2 := TBoldInternalObjectId.CreateWithClassID(1, True);
    try
      VS.EnsureObjectContents(Id1);
      VS.EnsureObjectContents(Id2);
      // Remove contents for Id2 but keep the ID in the list
      VS.RemoveFSObjectContentsByObjectId(Id2);
      VS.AllObjectIds(ResultList, True);
      // Only Id1 should be returned (it has contents loaded)
      Assert.AreEqual(1, ResultList.Count);
    finally
      Id1.Free;
      Id2.Free;
    end;
  finally
    VS.Free;
    ResultList.Free;
  end;
end;

procedure TTestBoldFSValueSpaceExtended.TestApplyValueSpace;
var
  VS1, VS2: TBoldFreeStandingValueSpace;
  Id: TBoldObjectId;
  OC: TBoldFreeStandingObjectContents;
  StrVal: IBoldStringContent;
begin
  VS1 := TBoldFreeStandingValueSpace.Create;
  VS2 := TBoldFreeStandingValueSpace.Create;
  try
    Id := TBoldInternalObjectId.CreateWithClassID(0, True);
    try
      // Setup source valuespace with a string member
      VS1.EnsureObjectContents(Id);
      OC := VS1.GetFSObjectContentsByObjectId(Id);
      OC.EnsureMemberAndGetValueByIndex(0, BoldContentName_String);
      (OC.ValueByIndex[0] as IBoldStringContent).SetContentAsString('Hello');
      OC.BoldPersistenceState := bvpsCurrent;
      // Apply to target
      VS2.ApplyValueSpace(VS1, True);
      Assert.IsTrue(VS2.GetHasContentsForId(Id));
      OC := VS2.GetFSObjectContentsByObjectId(Id);
      StrVal := OC.ValueByIndex[0] as IBoldStringContent;
      Assert.AreEqual('Hello', StrVal.AsString);
    finally
      Id.Free;
    end;
  finally
    FreeAfterTest(VS1);
    FreeAfterTest(VS2);
  end;
end;

procedure TTestBoldFSValueSpaceExtended.TestRemoveFSObjectContentsDirect;
var
  VS: TBoldFreeStandingValueSpace;
  Id: TBoldObjectId;
  OC: TBoldFreeStandingObjectContents;
begin
  VS := TBoldFreeStandingValueSpace.Create;
  try
    Id := TBoldInternalObjectId.CreateWithClassID(0, True);
    try
      VS.EnsureObjectContents(Id);
      OC := VS.GetFSObjectContentsByObjectId(Id);
      Assert.IsNotNull(OC);
      VS.RemoveFSObjectContents(OC);
      Assert.IsFalse(VS.GetHasContentsForId(Id));
    finally
      Id.Free;
    end;
  finally
    VS.Free;
  end;
end;

procedure TTestBoldFSValueSpaceExtended.TestGetEnsuredObjectContentsByObjectIdAndCheckIfCreated;
var
  VS: TBoldFreeStandingValueSpace;
  Id: TBoldObjectId;
  OC: IBoldObjectContents;
  WasCreated: Boolean;
begin
  VS := TBoldFreeStandingValueSpace.Create;
  try
    Id := TBoldInternalObjectId.CreateWithClassID(0, True);
    try
      WasCreated := VS.GetEnsuredObjectContentsByObjectIdAndCheckIfCreated(Id, OC);
      Assert.IsTrue(WasCreated);
      Assert.IsNotNull(OC);
      WasCreated := VS.GetEnsuredObjectContentsByObjectIdAndCheckIfCreated(Id, OC);
      Assert.IsFalse(WasCreated);
    finally
      Id.Free;
    end;
  finally
    FreeAfterTest(VS);
  end;
end;

procedure TTestBoldFSValueSpaceExtended.TestAssertLinkIntegrity;
var
  VS: TBoldFreeStandingValueSpace;
  Id: TBoldObjectId;
begin
  VS := TBoldFreeStandingValueSpace.Create;
  try
    Id := TBoldInternalObjectId.CreateWithClassID(0, True);
    try
      VS.EnsureObjectContents(Id);
      Assert.IsTrue(VS.AssertLinkIntegrity);
    finally
      Id.Free;
    end;
  finally
    VS.Free;
  end;
end;

procedure TTestBoldFSValueSpaceExtended.TestUpdateOwnValuesFrom;
var
  VS1, VS2: TBoldFreeStandingValueSpace;
  Id: TBoldObjectId;
  OC1, OC2: TBoldFreeStandingObjectContents;
begin
  VS1 := TBoldFreeStandingValueSpace.Create;
  VS2 := TBoldFreeStandingValueSpace.Create;
  try
    Id := TBoldInternalObjectId.CreateWithClassID(0, True);
    try
      // Setup VS1 (own) with a string member
      VS1.EnsureObjectContents(Id);
      OC1 := VS1.GetFSObjectContentsByObjectId(Id);
      OC1.EnsureMemberAndGetValueByIndex(0, BoldContentName_String);
      (OC1.ValueByIndex[0] as IBoldStringContent).SetContentAsString('Old');
      OC1.BoldPersistenceState := bvpsCurrent;
      // Setup VS2 (source) with updated value
      VS2.EnsureObjectContents(Id);
      OC2 := VS2.GetFSObjectContentsByObjectId(Id);
      OC2.EnsureMemberAndGetValueByIndex(0, BoldContentName_String);
      (OC2.ValueByIndex[0] as IBoldStringContent).SetContentAsString('New');
      OC2.BoldPersistenceState := bvpsCurrent;
      // Update own values from source
      VS1.UpdateOwnValuesFrom(VS2);
      OC1 := VS1.GetFSObjectContentsByObjectId(Id);
      Assert.AreEqual('New', (OC1.ValueByIndex[0] as IBoldStringContent).AsString);
    finally
      Id.Free;
    end;
  finally
    FreeAfterTest(VS1);
    FreeAfterTest(VS2);
  end;
end;

{ TTestBoldFSObjectContentsExtended }

procedure TTestBoldFSObjectContentsExtended.TestGetValueByMemberId;
var
  OC: TBoldFreeStandingObjectContents;
  MemberId: TBoldMemberId;
  V: IBoldValue;
begin
  OC := TBoldFreeStandingObjectContents.Create;
  try
    OC.EnsureMemberAndGetValueByIndex(3, BoldContentName_Integer);
    MemberId := TBoldMemberId.Create(3);
    try
      V := (OC as IBoldObjectContents).ValueByMemberId[MemberId];
      Assert.IsNotNull(V);
    finally
      MemberId.Free;
    end;
  finally
    FreeAfterTest(OC);
  end;
end;

procedure TTestBoldFSObjectContentsExtended.TestGetFSValueByIndex;
var
  OC: TBoldFreeStandingObjectContents;
  FSV: TBoldFreeStandingValue;
begin
  OC := TBoldFreeStandingObjectContents.Create;
  try
    OC.EnsureMemberAndGetValueByIndex(0, BoldContentName_String);
    FSV := OC.FSValueByIndex[0];
    Assert.IsNotNull(FSV);
  finally
    OC.Free;
  end;
end;

procedure TTestBoldFSObjectContentsExtended.TestGetFSValueByIndexOutOfRange;
var
  OC: TBoldFreeStandingObjectContents;
begin
  OC := TBoldFreeStandingObjectContents.Create;
  try
    Assert.IsNull(OC.FSValueByIndex[99]);
  finally
    OC.Free;
  end;
end;

procedure TTestBoldFSObjectContentsExtended.TestGetIsModified;
var
  OC: TBoldFreeStandingObjectContents;
begin
  OC := TBoldFreeStandingObjectContents.Create;
  try
    Assert.IsTrue((OC as IBoldObjectContents).IsModified);
  finally
    FreeAfterTest(OC);
  end;
end;

procedure TTestBoldFSObjectContentsExtended.TestIsReadOnly;
var
  OC: TBoldFreeStandingObjectContents;
  Intf: IBoldObjectContents;
begin
  OC := TBoldFreeStandingObjectContents.Create;
  try
    Intf := OC as IBoldObjectContents;
    Assert.IsFalse(Intf.IsReadOnly);
    Intf.IsReadOnly := True;
    Assert.IsTrue(Intf.IsReadOnly);
  finally
    FreeAfterTest(OC);
  end;
end;

procedure TTestBoldFSObjectContentsExtended.TestGlobalId;
var
  OC: TBoldFreeStandingObjectContents;
  Intf: IBoldObjectContents;
begin
  OC := TBoldFreeStandingObjectContents.Create;
  try
    Intf := OC as IBoldObjectContents;
    Assert.AreEqual('', Intf.GlobalId);
    Intf.GlobalId := 'test-global-id';
    Assert.AreEqual('test-global-id', Intf.GlobalId);
  finally
    FreeAfterTest(OC);
  end;
end;

procedure TTestBoldFSObjectContentsExtended.TestObjectContentsContentType;
begin
  Assert.AreEqual(Ord(bctObject), Ord(TBoldFreeStandingObjectContents.ContentType));
end;

procedure TTestBoldFSObjectContentsExtended.TestGetStreamName;
var
  OC: TBoldFreeStandingObjectContents;
begin
  OC := TBoldFreeStandingObjectContents.Create;
  try
    Assert.AreEqual(BOLDOBJECTCONTENTSNAME, (OC as IBoldStreamable).StreamName);
  finally
    FreeAfterTest(OC);
  end;
end;

procedure TTestBoldFSObjectContentsExtended.TestIsEmptyWithMembers;
var
  OC: TBoldFreeStandingObjectContents;
begin
  OC := TBoldFreeStandingObjectContents.Create;
  try
    Assert.IsTrue(OC.IsEmpty);
    OC.EnsureMemberAndGetValueByIndex(0, BoldContentName_String);
    Assert.IsFalse(OC.IsEmpty);
  finally
    OC.Free;
  end;
end;

procedure TTestBoldFSObjectContentsExtended.TestApplyObjectContentsIgnorePersistenceState;
var
  OC1, OC2: TBoldFreeStandingObjectContents;
  StrVal: IBoldStringContent;
begin
  OC1 := TBoldFreeStandingObjectContents.Create;
  OC2 := TBoldFreeStandingObjectContents.Create;
  try
    // Setup source
    OC1.EnsureMemberAndGetValueByIndex(0, BoldContentName_String);
    (OC1.ValueByIndex[0] as IBoldStringContent).SetContentAsString('Test');
    OC1.BoldPersistenceState := bvpsCurrent;
    // Apply with IgnorePersistenceState=True
    OC2.ApplyObjectContents(OC1, True, True);
    StrVal := OC2.ValueByIndex[0] as IBoldStringContent;
    Assert.AreEqual('Test', StrVal.AsString);
  finally
    FreeAfterTest(OC1);
    FreeAfterTest(OC2);
  end;
end;

procedure TTestBoldFSObjectContentsExtended.TestObjectIdRefApplyTranslationList;
var
  V: TBFSObjectIdRef;
  OldId, NewId: TBoldInternalObjectId;
begin
  // Test SetFromId with Adopt=True when replacing an existing ID
  V := TBFSObjectIdRef.Create;
  try
    OldId := TBoldInternalObjectId.CreateWithClassID(5, True);
    NewId := TBoldInternalObjectId.CreateWithClassID(50, True);
    V.SetFromId(OldId, True); // adopt OldId
    Assert.IsTrue(V.Id.IsEqual[OldId]);
    V.SetFromId(NewId, True); // adopt NewId, frees OldId
    Assert.IsTrue(V.Id.IsEqual[NewId]);
    // Don't free OldId/NewId - owned by V
  finally
    V.Free;
  end;
end;

procedure TTestBoldFSObjectContentsExtended.TestObjectIdListRefApplyTranslationList;
var
  V: TBFSObjectIdListRef;
  Id1, Id2: TBoldObjectId;
begin
  // Test GetContentAsString for ObjectIdListRef
  V := TBFSObjectIdListRef.Create;
  try
    Id1 := TBoldInternalObjectId.CreateWithClassID(5, True);
    Id2 := TBoldInternalObjectId.CreateWithClassID(10, True);
    try
      (V as IBoldFreeStandingIdList).AddId(Id1);
      (V as IBoldFreeStandingIdList).AddId(Id2);
      Assert.AreEqual(2, V.Count);
      Assert.IsNotEmpty((V as IBoldStringRepresentable).AsString);
    finally
      Id1.Free;
      Id2.Free;
    end;
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFSObjectContentsExtended.TestObjectIdRefGetContentAsString;
var
  V: TBFSObjectIdRef;
  StrRep: IBoldStringRepresentable;
  Id: TBoldInternalObjectId;
begin
  V := TBFSObjectIdRef.Create;
  try
    Assert.AreEqual('<nil>', (V as IBoldStringRepresentable).StringRepresentation[0]);
    Id := TBoldInternalObjectId.CreateWithClassID(10, True);
    try
      V.SetFromId(Id, False);
      StrRep := V as IBoldStringRepresentable;
      Assert.IsNotEmpty(StrRep.AsString);
    finally
      Id.Free;
    end;
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFSObjectContentsExtended.TestObjectIdRefGetStringRepresentationNil;
var
  V: TBFSObjectIdRef;
begin
  V := TBFSObjectIdRef.Create;
  try
    Assert.AreEqual('<nil>', (V as IBoldStringRepresentable).StringRepresentation[0]);
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFSObjectContentsExtended.TestObjectIdListRefGetStringRepresentation;
var
  V: TBFSObjectIdListRef;
  Id: TBoldObjectId;
begin
  V := TBFSObjectIdListRef.Create;
  try
    Id := TBoldInternalObjectId.CreateWithClassID(1, True);
    try
      (V as IBoldFreeStandingIdList).AddId(Id);
      Assert.IsNotEmpty((V as IBoldStringRepresentable).StringRepresentation[0]);
    finally
      Id.Free;
    end;
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFSObjectContentsExtended.TestObjectIdListRefSetList;
var
  V: TBFSObjectIdListRef;
  SourceList, TargetList: TBoldObjectIdList;
  Id1, Id2: TBoldObjectId;
begin
  V := TBFSObjectIdListRef.Create;
  SourceList := TBoldObjectIdList.Create;
  TargetList := TBoldObjectIdList.Create;
  try
    Id1 := TBoldInternalObjectId.CreateWithClassID(1, True);
    Id2 := TBoldInternalObjectId.CreateWithClassID(2, True);
    try
      SourceList.Add(Id1);
      SourceList.Add(Id2);
      V.SetFromIdList(SourceList);
      V.SetList(TargetList);
      Assert.AreEqual(2, TargetList.Count);
    finally
      Id1.Free;
      Id2.Free;
    end;
  finally
    V.Free;
    SourceList.Free;
    TargetList.Free;
  end;
end;

procedure TTestBoldFSObjectContentsExtended.TestObjectIdListRefPairGetIdLists;
var
  V: TBFSObjectIdListRefPair;
  IdList1, IdList2: TBoldObjectIdList;
  Id1, Id2: TBoldObjectId;
begin
  V := TBFSObjectIdListRefPair.Create;
  IdList1 := TBoldObjectIdList.Create;
  IdList2 := TBoldObjectIdList.Create;
  try
    Id1 := TBoldInternalObjectId.CreateWithClassID(10, True);
    Id2 := TBoldInternalObjectId.CreateWithClassID(20, True);
    try
      IdList1.Add(Id1);
      IdList2.Add(Id2);
      V.SetFromIdLists(IdList1, IdList2);
      Assert.IsTrue(V.IdList1[0].IsEqual[Id1]);
      Assert.IsTrue(V.IdList2[0].IsEqual[Id2]);
      Assert.IsNotEmpty((V as IBoldStringRepresentable).StringRepresentation[0]);
      Assert.IsNotEmpty((V as IBoldStringRepresentable).AsString);
    finally
      Id1.Free;
      Id2.Free;
    end;
  finally
    FreeAfterTest(V);
    FreeAfterTest(IdList1);
    FreeAfterTest(IdList2);
  end;
end;

procedure TTestBoldFSObjectContentsExtended.TestObjectIdListRefPairRemoveId;
var
  V: TBFSObjectIdListRefPair;
  Id1, Id2: TBoldObjectId;
  Pair: IBoldFreeStandingIdListPair;
begin
  V := TBFSObjectIdListRefPair.Create;
  try
    Id1 := TBoldInternalObjectId.CreateWithClassID(1, True);
    Id2 := TBoldInternalObjectId.CreateWithClassID(2, True);
    try
      Pair := V as IBoldFreeStandingIdListPair;
      Pair.AddIds(Id1, Id2);
      Assert.AreEqual(1, V.Count);
      Pair.RemoveId(Id1);
      Assert.AreEqual(0, V.Count);
    finally
      Id1.Free;
      Id2.Free;
    end;
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFSObjectContentsExtended.TestObjectIdListRefPairAssignContent;
var
  V1, V2: TBFSObjectIdListRefPair;
  Id1, Id2: TBoldObjectId;
begin
  V1 := TBFSObjectIdListRefPair.Create;
  V2 := TBFSObjectIdListRefPair.Create;
  try
    Id1 := TBoldInternalObjectId.CreateWithClassID(10, True);
    Id2 := TBoldInternalObjectId.CreateWithClassID(20, True);
    try
      (V1 as IBoldFreeStandingIdListPair).AddIds(Id1, Id2);
      V2.AssignContent(V1 as IBoldValue);
      Assert.AreEqual(1, V2.Count);
      Assert.IsTrue(V2.IdList1[0].IsEqual[Id1]);
      Assert.IsTrue(V2.IdList2[0].IsEqual[Id2]);
    finally
      Id1.Free;
      Id2.Free;
    end;
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFSObjectContentsExtended.TestObjectIdListRefAssignContent;
var
  V1, V2: TBFSObjectIdListRef;
  Id: TBoldObjectId;
begin
  V1 := TBFSObjectIdListRef.Create;
  V2 := TBFSObjectIdListRef.Create;
  try
    Id := TBoldInternalObjectId.CreateWithClassID(5, True);
    try
      (V1 as IBoldFreeStandingIdList).AddId(Id);
      V2.AssignContent(V1 as IBoldValue);
      Assert.AreEqual(1, V2.Count);
    finally
      Id.Free;
    end;
  finally
    FreeAfterTest(V1);
    FreeAfterTest(V2);
  end;
end;

procedure TTestBoldFSObjectContentsExtended.TestGetObjectId;
var
  VS: TBoldFreeStandingValueSpace;
  Id: TBoldObjectId;
  OC: TBoldFreeStandingObjectContents;
begin
  VS := TBoldFreeStandingValueSpace.Create;
  try
    Id := TBoldInternalObjectId.CreateWithClassID(42, True);
    try
      VS.EnsureObjectContents(Id);
      OC := VS.GetFSObjectContentsByObjectId(Id);
      Assert.IsNotNull(OC.ObjectId);
      Assert.IsTrue(OC.ObjectId.IsEqual[Id]);
    finally
      Id.Free;
    end;
  finally
    VS.Free;
  end;
end;

procedure TTestBoldFSObjectContentsExtended.TestUpdateObjectContentsFrom;
var
  OC1, OC2: TBoldFreeStandingObjectContents;
begin
  OC1 := TBoldFreeStandingObjectContents.Create;
  OC2 := TBoldFreeStandingObjectContents.Create;
  try
    // Setup OC1 (target) with a string member
    OC1.EnsureMemberAndGetValueByIndex(0, BoldContentName_String);
    (OC1.ValueByIndex[0] as IBoldStringContent).SetContentAsString('Original');
    OC1.BoldPersistenceState := bvpsCurrent;
    // Setup OC2 (source) with updated value
    OC2.EnsureMemberAndGetValueByIndex(0, BoldContentName_String);
    (OC2.ValueByIndex[0] as IBoldStringContent).SetContentAsString('Updated');
    OC2.BoldPersistenceState := bvpsModified;
    // Update from source
    OC1.UpdateObjectContentsFrom(OC2);
    Assert.AreEqual('Updated', (OC1.ValueByIndex[0] as IBoldStringContent).AsString);
    Assert.AreEqual(Ord(bvpsModified), Ord(OC1.BoldPersistenceState));
  finally
    FreeAfterTest(OC1);
    FreeAfterTest(OC2);
  end;
end;

procedure TTestBoldFSObjectContentsExtended.TestObjectIdRefGetStreamName;
var V: TBFSObjectIdRef;
begin
  V := TBFSObjectIdRef.Create;
  try
    Assert.AreEqual(BoldContentName_ObjectIdRef, (V as IBoldStreamable).StreamName);
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFSObjectContentsExtended.TestObjectIdListRefGetStreamName;
var V: TBFSObjectIdListRef;
begin
  V := TBFSObjectIdListRef.Create;
  try
    Assert.AreEqual(BoldContentName_ObjectIdListRef, (V as IBoldStreamable).StreamName);
  finally
    FreeAfterTest(V);
  end;
end;

procedure TTestBoldFSObjectContentsExtended.TestObjectIdListRefPairGetStreamName;
var V: TBFSObjectIdListRefPair;
begin
  V := TBFSObjectIdListRefPair.Create;
  try
    Assert.AreEqual(BoldContentName_ObjectIdListRefPair, (V as IBoldStreamable).StreamName);
  finally
    FreeAfterTest(V);
  end;
end;

initialization
  BoldRegisterTest(TTestBoldFreeStandingValueSpace);
  BoldRegisterTest(TTestBoldFreeStandingNullableValues);
  BoldRegisterTest(TTestBoldFreeStandingIdRefs);
  BoldRegisterTest(TTestBoldFreeStandingValues);
  BoldRegisterTest(TTestBoldFSValueSpaceExtended);
  BoldRegisterTest(TTestBoldFSObjectContentsExtended);

end.
