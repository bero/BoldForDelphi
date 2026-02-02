unit Test.BoldFreeStandingValues;

interface

uses
  DUnitX.TestFramework,
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
  [TestFixture]
  [Category('FreeStandingValues')]
  TTestBoldFreeStandingValueSpace = class
  public
    // TBoldFreeStandingValueSpace tests
    [Test]
    procedure TestValueSpaceCreate;
    [Test]
    procedure TestValueSpaceIsEmptyInitially;
    [Test]
    procedure TestValueSpaceIdCountZeroInitially;
    [Test]
    procedure TestValueSpaceEnsureObjectContents;
    [Test]
    procedure TestValueSpaceGetHasContentsForId;
    [Test]
    procedure TestValueSpaceGetFSObjectContentsByObjectId;
    [Test]
    procedure TestValueSpaceRemoveFSObjectContents;
    [Test]
    procedure TestValueSpaceAllObjectIds;
    [Test]
    procedure TestValueSpaceClear;
    [Test]
    procedure TestValueSpaceGetAnyObjectId;
    [Test]
    procedure TestValueSpaceContentType;

    // TBoldFreeStandingObjectContents tests
    [Test]
    procedure TestObjectContentsCreate;
    [Test]
    procedure TestObjectContentsIsEmptyInitially;
    [Test]
    procedure TestObjectContentsMemberCountZero;
    [Test]
    procedure TestObjectContentsEnsureMemberAndGetValueByIndex;
    [Test]
    procedure TestObjectContentsGetValueByIndex;
    [Test]
    procedure TestObjectContentsExistenceState;
    [Test]
    procedure TestObjectContentsPersistenceState;
    [Test]
    procedure TestObjectContentsTimeStamp;
    [Test]
    procedure TestObjectContentsMarkAllMembersCurrent;
    [Test]
    procedure TestObjectContentsRemoveMemberByIndex;
  end;

  { Tests for nullable value types (TBFSInteger, TBFSString, etc.) }
  [TestFixture]
  [Category('FreeStandingValues')]
  TTestBoldFreeStandingNullableValues = class
  public
    // TBFSInteger tests
    [Test]
    procedure TestIntegerCreate;
    [Test]
    procedure TestIntegerSetAndGet;
    [Test]
    procedure TestIntegerIsNullInitially;
    [Test]
    procedure TestIntegerSetToNonNull;
    [Test]
    procedure TestIntegerContentType;
    [Test]
    procedure TestIntegerIsEqualToValue;
    [Test]
    procedure TestIntegerAssignContent;

    // TBFSString tests
    [Test]
    procedure TestStringCreate;
    [Test]
    procedure TestStringSetAndGet;
    [Test]
    procedure TestStringIsNullInitially;
    [Test]
    procedure TestStringContentType;
    [Test]
    procedure TestStringIsEqualToValue;
    [Test]
    procedure TestStringAssignContent;

    // TBFSFloat tests
    [Test]
    procedure TestFloatCreate;
    [Test]
    procedure TestFloatSetAndGet;
    [Test]
    procedure TestFloatContentType;
    [Test]
    procedure TestFloatIsEqualToValue;

    // TBFSCurrency tests
    [Test]
    procedure TestCurrencyCreate;
    [Test]
    procedure TestCurrencySetAndGet;
    [Test]
    procedure TestCurrencyContentType;

    // TBFSBoolean tests
    [Test]
    procedure TestBooleanCreate;
    [Test]
    procedure TestBooleanSetAndGet;
    [Test]
    procedure TestBooleanContentType;

    // TBFSDateTime tests
    [Test]
    procedure TestDateTimeCreate;
    [Test]
    procedure TestDateTimeSetAndGet;
    [Test]
    procedure TestDateTimeContentType;

    // TBFSDate tests
    [Test]
    procedure TestDateCreate;
    [Test]
    procedure TestDateSetAndGet;
    [Test]
    procedure TestDateContentType;

    // TBFSTime tests
    [Test]
    procedure TestTimeCreate;
    [Test]
    procedure TestTimeSetAndGet;
    [Test]
    procedure TestTimeContentType;

    // TBFSBlob tests
    [Test]
    procedure TestBlobCreate;
    [Test]
    procedure TestBlobSetAndGet;
    [Test]
    procedure TestBlobContentType;

    // TBFSTypedBlob tests
    [Test]
    procedure TestTypedBlobCreate;
    [Test]
    procedure TestTypedBlobContentTypeProperty;
    [Test]
    procedure TestTypedBlobContentType;
  end;

  { Tests for TBFSObjectIdRef and list types }
  [TestFixture]
  [Category('FreeStandingValues')]
  TTestBoldFreeStandingIdRefs = class
  public
    // TBFSObjectIdRef tests
    [Test]
    procedure TestObjectIdRefCreate;
    [Test]
    procedure TestObjectIdRefSetFromId;
    [Test]
    procedure TestObjectIdRefSetFromIdAdopt;
    [Test]
    procedure TestObjectIdRefContentType;
    [Test]
    procedure TestObjectIdRefGetStringRepresentation;
    [Test]
    procedure TestObjectIdRefIsEqualToValue;
    [Test]
    procedure TestObjectIdRefAssignContent;

    // TBFSObjectIdListRef tests
    [Test]
    procedure TestObjectIdListRefCreate;
    [Test]
    procedure TestObjectIdListRefCountZero;
    [Test]
    procedure TestObjectIdListRefSetFromIdList;
    [Test]
    procedure TestObjectIdListRefContentType;
    [Test]
    procedure TestObjectIdListRefAddAndRemoveId;

    // TBFSObjectIdListRefPair tests
    [Test]
    procedure TestObjectIdListRefPairCreate;
    [Test]
    procedure TestObjectIdListRefPairSetFromIdLists;
    [Test]
    procedure TestObjectIdListRefPairContentType;
    [Test]
    procedure TestObjectIdListRefPairAddIds;
  end;

  [TestFixture]
  [Category('FreeStandingValues')]
  TTestBoldFreeStandingValues = class
  public
    // Basic empty state tests
    [Test]
    [Category('Quick')]
    procedure TestObjectIdRefPairGetId1Empty;
    [Test]
    [Category('Quick')]
    procedure TestObjectIdRefPairGetId2Empty;

    // SetFromIds tests
    [Test]
    [Category('Quick')]
    procedure TestObjectIdRefPairGetId1WithOneId;
    [Test]
    [Category('Quick')]
    procedure TestObjectIdRefPairGetId1And2WithTwoIds;
    [Test]
    [Category('Quick')]
    procedure TestSetFromIdsVerifyActualIdValues;
    [Test]
    [Category('Quick')]
    procedure TestSetFromIdsClonesIds;
    [Test]
    [Category('Quick')]
    procedure TestSetFromIdsReplacesExistingIds;
    [Test]
    [Category('Quick')]
    procedure TestSetFromIdsWithBothNil;

    // OrderNo tests
    [Test]
    [Category('Quick')]
    procedure TestOrderNoDefaultValue;
    [Test]
    [Category('Quick')]
    procedure TestOrderNoSetAndGet;

    // ContentType and StreamName tests
    [Test]
    [Category('Quick')]
    procedure TestContentTypeReturnsObjectIdRefPair;
    [Test]
    [Category('Quick')]
    procedure TestGetStreamNameReturnsCorrectValue;

    // AssignContentValue tests
    [Test]
    [Category('Quick')]
    procedure TestAssignContentValueCopiesIds;
    [Test]
    [Category('Quick')]
    procedure TestAssignContentValueCopiesOrderNo;
    [Test]
    [Category('Quick')]
    procedure TestAssignContentValueWithNilIds;

    // ApplyTranslationList tests
    [Test]
    [Category('Quick')]
    procedure TestApplyTranslationListTranslatesIds;
    [Test]
    [Category('Quick')]
    procedure TestApplyTranslationListWithNoMatchingIds;
    [Test]
    [Category('Quick')]
    procedure TestApplyTranslationListWithNilIdList;

    // GetStringRepresentation and GetContentAsString tests (bug fix tests)
    [Test]
    [Category('Quick')]
    procedure TestGetStringRepresentationWithTwoIds;
    [Test]
    [Category('Quick')]
    procedure TestGetStringRepresentationWithOneId;
    [Test]
    [Category('Quick')]
    procedure TestGetStringRepresentationEmpty;
    [Test]
    [Category('Quick')]
    procedure TestGetContentAsString;
    [Test]
    [Category('Quick')]
    procedure TestIBoldStringRepresentableInterface;
  end;

implementation

uses
  SysUtils,
  BoldDefs,
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
    OC.Free;
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
    OC.Free;
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
    OC.Free;
  end;
end;

procedure TTestBoldFreeStandingValueSpace.TestObjectContentsRemoveMemberByIndex;
var
  OC: TBoldFreeStandingObjectContents;
begin
  OC := TBoldFreeStandingObjectContents.Create;
  try
    OC.EnsureMemberAndGetValueByIndex(0, 'String');
    Assert.IsNotNull(OC.ValueByIndex[0], 'Value should exist');
    OC.RemoveMemberByIndex(0);
    Assert.IsNull(OC.ValueByIndex[0], 'Value should be nil after remove');
  finally
    OC.Free;
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
    V.Free;
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
    V.Free;
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
    V1.Free;
    V2.Free;
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
    V1.Free;
    V2.Free;
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
    V.Free;
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
    V1.Free;
    V2.Free;
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
    V1.Free;
    V2.Free;
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
    V1.Free;
    V2.Free;
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
    Assert.AreEqual(123.45, Double(V.AsCurrency), 0.001, 'AsCurrency should be 123.45');
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
    V.Free;
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
    V1.Free;
    V2.Free;
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
    V1.Free;
    V2.Free;
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
    V.Free;
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
    V.Free;
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
    IdRefPair.Free;
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
    Source.Free;
    Target.Free;
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
    Source.Free;
    Target.Free;
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
    Source.Free;
    Target.Free;
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
    IdRefPair.Free;
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
    IdRefPair.Free;
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
    IdRefPair.Free;
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
    IdRefPair.Free;
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
    IdRefPair.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldFreeStandingValueSpace);
  TDUnitX.RegisterTestFixture(TTestBoldFreeStandingNullableValues);
  TDUnitX.RegisterTestFixture(TTestBoldFreeStandingIdRefs);
  TDUnitX.RegisterTestFixture(TTestBoldFreeStandingValues);

end.
