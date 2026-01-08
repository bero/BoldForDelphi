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
  BoldDefaultStreamNames;

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
  TDUnitX.RegisterTestFixture(TTestBoldFreeStandingValues);

end.
