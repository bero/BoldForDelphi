unit Test.BoldAttributes;

{ DUnitX version of dmjehoBoldTest - Bold Attribute Tests }

interface

uses
  Classes,
  Forms,
  DUnitX.TestFramework,
  BoldTestCase,
  jehoBCBoldTest,
  BoldSystem,
  BoldHandle,
  BoldHandles,
  BoldSystemHandle,
  BoldSubscription,
  BoldModel,
  BoldTypeNameHandle,
  BoldDefs,
  BoldAttributes,
  BoldAbstractModel;

type
  TCompareTypeSet = set of TBoldCompareType;

  TjehodmBoldTest = class(TDataModule)
    BoldModel1: TBoldModel;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldTypeNameHandle1: TBoldTypeNameHandle;
  end;

  [TestFixture]
  [Category('ObjectSpace')]
  TTestBoldAttributes = class(TBoldTestCase)
  private
    FDataModule: TjehodmBoldTest;
  protected
    procedure SetAttributeValues(ObjectClassA: TClassA);
    procedure CheckAttributes(ObjectClassA: TClassA);
    procedure NullAttributeValues(ObjectClassA: TClassA);
    procedure CheckNullAttributes(ObjectClassA: TClassA);
    procedure CheckAttributeIndexes(ObjectClassA: TClassA);
    procedure AccessMember(BoldMember: TBoldMember);
    procedure CompareMembers(CompareMember, CompareWith: TBoldMember);
    procedure CheckDerivedAttributes(ObjectSubA: TClassDerivedA);
    procedure CheckOclDerivedAttributes(ObjectOclSubA: TClassOclDerivedA);
    // CompareToAs helpers
    procedure DoTestString(OperandA, OperandB: TBoldAttribute);
    procedure DoTestInteger(OperandA, OperandB: TBAInteger);
    procedure DoTestCurrency(OperandA, OperandB: TBACurrency);
    procedure DoTestDate(OperandA, OperandB: TBADate);
    procedure DoTestDateTime(OperandA, OperandB: TBADateTime);
    procedure DoTestTime(OperandA, OperandB: TBATime);
    procedure DoTestFloat(OperandA, OperandB: TBAFloat);
    procedure DoTestCompareToAs(Op1, Op2: TBoldAttribute; Expected: Integer);
    procedure DoCheckCompareTypes(OperandA, OperandB: TBoldAttribute; ValidCompareTypes: TCompareTypeSet);
    procedure InternalCheckCompareTypes(OperandA, OperandB: TBoldAttribute; CompareType: TBoldCompareType; WillWork: Boolean);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    [Test]
    procedure AttributeValueTest;
    [Test]
    procedure AttributeNullTest;
    [Test]
    procedure AttributeIndexTest;
    [Test]
    procedure AttributeDerivationTest;
    [Test]
    procedure AttributeOclDerivationTest;
    [Test]
    procedure CheckMultiDerivedAttributes;
    [Test]
    procedure CompareToAsTest;
    [Test]
    procedure CompareToAsNullTest;
    [Test]
    procedure TestCompareTypes;
  end;

var
  jehodmBoldTest: TjehodmBoldTest;

implementation

uses
  SysUtils;

{$R dmjehoBoldTest.dfm}

const
  FLOATCONST: Real = 0.1;
  INTEGERCONST: Integer = 1;
  STRINGCONST: string = 'a';
  BOOLEANCONST: Boolean = True;
  CURRENCYCONST: Currency = 0.1;
  NROFATTRIBUTES = 14;

{ TTestBoldAttributes }

procedure TTestBoldAttributes.SetUp;
begin
  inherited;
  FDataModule := TjehodmBoldTest.Create(nil);
  jehodmBoldTest := FDataModule;
end;

procedure TTestBoldAttributes.TearDown;
begin
  FreeAndNil(FDataModule);
  jehodmBoldTest := nil;
  inherited;
end;

procedure TTestBoldAttributes.SetAttributeValues(ObjectClassA: TClassA);
begin
  with ObjectClassA do
  begin
    aInteger := INTEGERCONST;
    aFloat := FLOATCONST;
    aSmallInt := INTEGERCONST;
    aCurrency := CURRENCYCONST;
    aTime := FLOATCONST;
    aWord := INTEGERCONST;
    aString := STRINGCONST;
    aDateTime := FLOATCONST;
    aByte := INTEGERCONST;
    aDate := INTEGERCONST;
    aBoolean := BOOLEANCONST;
    aShortInt := INTEGERCONST;
    aBlob := STRINGCONST;
    aBlobContent := STRINGCONST;
  end;
end;

procedure TTestBoldAttributes.CheckAttributes(ObjectClassA: TClassA);
begin
  Assert.AreEqual(INTEGERCONST, ObjectClassA.aInteger, 'Integer');
  Assert.AreEqual(FLOATCONST, ObjectClassA.aFloat, 0.0001, 'Float');
  Assert.AreEqual(SmallInt(INTEGERCONST), ObjectClassA.aSmallInt, 'SmallInt');
  Assert.AreEqual(CURRENCYCONST, ObjectClassA.aCurrency, 'Currency');
  Assert.AreEqual(FLOATCONST, Double(ObjectClassA.aTime), 0.0001, 'Time');
  Assert.AreEqual(Word(INTEGERCONST), ObjectClassA.aWord, 'Word');
  Assert.AreEqual(STRINGCONST, ObjectClassA.aString, 'String');
  Assert.AreEqual(FLOATCONST, ObjectClassA.aDateTime, 0.0001, 'DateTime');
  Assert.AreEqual(Byte(INTEGERCONST), ObjectClassA.aByte, 'Byte');
  Assert.AreEqual(Double(INTEGERCONST), Double(ObjectClassA.aDate), 0.0001, 'Date');
  Assert.AreEqual(BOOLEANCONST, ObjectClassA.aBoolean, 'Boolean');
  Assert.AreEqual(ShortInt(INTEGERCONST), ObjectClassA.aShortInt, 'ShortInt');
  Assert.AreEqual(STRINGCONST, ObjectClassA.aBlob, 'Blob');
  Assert.AreEqual(STRINGCONST, ObjectClassA.aBlobContent, 'BlobContent');
end;

procedure TTestBoldAttributes.NullAttributeValues(ObjectClassA: TClassA);
begin
  with ObjectClassA do
  begin
    m_aInteger.SetToNull;
    m_aString.SetToNull;
    m_aBoolean.SetToNull;
    m_aByte.SetToNull;
    m_aCurrency.SetToNull;
    m_aTime.SetToNull;
    m_aDate.SetToNull;
    m_aFloat.SetToNull;
    m_aDateTime.SetToNull;
    m_aWord.SetToNull;
    m_aShortInt.SetToNull;
    m_aSmallInt.SetToNull;
    m_aBlob.SetToNull;
    m_aBlobContent.SetToNull;
  end;
end;

procedure TTestBoldAttributes.CheckNullAttributes(ObjectClassA: TClassA);
begin
  with ObjectClassA do
  begin
    Assert.IsTrue(m_aInteger.IsNull, 'm_aInteger');
    Assert.IsTrue(m_aFloat.IsNull, 'm_aFloat');
    Assert.IsTrue(m_aSmallInt.IsNull, 'm_aSmallInt');
    Assert.IsTrue(m_aCurrency.IsNull, 'm_aCurrency');
    Assert.IsTrue(m_aTime.IsNull, 'm_aTime');
    Assert.IsTrue(m_aWord.IsNull, 'm_aWord');
    Assert.IsTrue(m_aString.IsNull, 'm_aString');
    Assert.IsTrue(m_aDateTime.IsNull, 'm_aDateTime');
    Assert.IsTrue(m_aByte.IsNull, 'm_aByte');
    Assert.IsTrue(m_aDate.IsNull, 'm_aDate');
    Assert.IsTrue(m_aBoolean.IsNull, 'm_aBoolean');
    Assert.IsTrue(m_aShortInt.IsNull, 'm_aShortInt');
    Assert.IsTrue(m_aBlob.IsNull, 'm_aBlob');
    Assert.IsTrue(m_aBlobContent.IsNull, 'm_aBlobContent');
  end;
end;

procedure TTestBoldAttributes.AttributeValueTest;
var
  ObjectA: TClassA;
begin
  ObjectA := TClassA.Create(nil);
  try
    SetAttributeValues(ObjectA);
    CheckAttributes(ObjectA);
  finally
    ObjectA.Delete;
  end;
end;

procedure TTestBoldAttributes.AttributeNullTest;
var
  ObjectA: TClassA;
begin
  ObjectA := TClassA.Create(nil);
  try
    NullAttributeValues(ObjectA);
    CheckNullAttributes(ObjectA);
  finally
    ObjectA.Delete;
  end;
end;

procedure TTestBoldAttributes.AttributeIndexTest;
var
  ObjectA: TClassA;
begin
  ObjectA := TClassA.Create(nil);
  try
    CheckAttributeIndexes(ObjectA);
  finally
    ObjectA.Delete;
  end;
end;

procedure TTestBoldAttributes.CheckAttributeIndexes(ObjectClassA: TClassA);
begin
  Assert.AreEqual(NROFATTRIBUTES, ObjectClassA.BoldMemberCount, 'Wrong number of attributes');

  with ObjectClassA do
  begin
    AccessMember(m_aString);
    AccessMember(m_aBoolean);
    AccessMember(m_aByte);
    AccessMember(m_aCurrency);
    AccessMember(m_aDate);
    AccessMember(m_aDateTime);
    AccessMember(m_aFloat);
    AccessMember(m_aInteger);
    AccessMember(m_aShortInt);
    AccessMember(m_aSmallInt);
    AccessMember(m_aTime);
    AccessMember(m_aWord);
    AccessMember(m_aBlob);
    AccessMember(m_aBlobContent);
  end;
end;

procedure TTestBoldAttributes.AccessMember(BoldMember: TBoldMember);
begin
  Assert.IsTrue(Assigned(BoldMember.BoldSystem), Format('%s has no system', [BoldMember.ClassName]));
end;

procedure TTestBoldAttributes.AttributeDerivationTest;
var
  ObjectSubA: TClassDerivedA;
begin
  ObjectSubA := TClassDerivedA.Create(nil);
  try
    SetAttributeValues(ObjectSubA);
    CheckDerivedAttributes(ObjectSubA);
  finally
    ObjectSubA.Delete;
  end;
end;

procedure TTestBoldAttributes.CheckDerivedAttributes(ObjectSubA: TClassDerivedA);
begin
  with ObjectSubA do
  begin
    CompareMembers(m_aDerivedInteger, m_aInteger);
    CompareMembers(m_aDerivedFloat, m_aFloat);
    CompareMembers(m_aDerivedSmallInt, m_aSmallInt);
    CompareMembers(m_aDerivedCurrency, m_aCurrency);
    CompareMembers(m_aDerivedTime, m_aTime);
    CompareMembers(m_aDerivedWord, m_aWord);
    CompareMembers(m_aDerivedString, m_aString);
    CompareMembers(m_aDerivedDateTime, m_aDateTime);
    CompareMembers(m_aDerivedByte, m_aByte);
    CompareMembers(m_aDerivedDate, m_aDate);
    CompareMembers(m_aDerivedBoolean, m_aBoolean);
    CompareMembers(m_aDerivedBlob, m_aBlob);
    CompareMembers(m_aDerivedBlobContent, m_aBlobContent);
  end;
end;

procedure TTestBoldAttributes.CompareMembers(CompareMember, CompareWith: TBoldMember);
begin
  Assert.IsTrue(CompareMember.IsEqual(CompareWith),
    Format('Derived attribute %s did not work', [CompareMember.ClassName]));
end;

procedure TTestBoldAttributes.AttributeOclDerivationTest;
var
  ObjectOclSubA: TClassOclDerivedA;
begin
  ObjectOclSubA := TClassOclDerivedA.Create(nil);
  try
    SetAttributeValues(ObjectOclSubA);
    CheckOclDerivedAttributes(ObjectOclSubA);
  finally
    ObjectOclSubA.Delete;
  end;
end;

procedure TTestBoldAttributes.CheckOclDerivedAttributes(ObjectOclSubA: TClassOclDerivedA);
begin
  with ObjectOclSubA do
  begin
    CompareMembers(m_aOclDerivedInteger, m_aInteger);
    CompareMembers(m_aOclDerivedFloat, m_aFloat);
    CompareMembers(m_aOclDerivedSmallInt, m_aSmallInt);
    CompareMembers(m_aOclDerivedCurrency, m_aCurrency);
    CompareMembers(m_aOclDerivedTime, m_aTime);
    CompareMembers(m_aOclDerivedWord, m_aWord);
    CompareMembers(m_aOclDerivedString, m_aString);
    CompareMembers(m_aOclDerivedDateTime, m_aDateTime);
    CompareMembers(m_aOclDerivedByte, m_aByte);
    CompareMembers(m_aOclDerivedDate, m_aDate);
    CompareMembers(m_aOclDerivedBoolean, m_aBoolean);
    CompareMembers(m_aOclDerivedBlob, m_aBlob);
    CompareMembers(m_aOclDerivedBlobContent, m_aBlobContent);
  end;
end;

procedure TTestBoldAttributes.CheckMultiDerivedAttributes;
var
  ObjectMultiOclSubA: TClassMultiDerivedA;
begin
  ObjectMultiOclSubA := TClassMultiDerivedA.Create(nil);
  try
    SetAttributeValues(ObjectMultiOclSubA);
    with ObjectMultiOclSubA do
    begin
      Assert.AreEqual(0, m_aMultiDerivedBoolean.CompareToAs(ctDefault, m_aOclDerivedBoolean), 'MultiDerivedBoolean');
      Assert.AreEqual(0, m_aMultiDerivedByte.CompareToAs(ctDefault, m_aOclDerivedByte), 'MultiDerivedByte');
      Assert.AreEqual(0, m_aMultiDerivedCurrency.CompareToAs(ctDefault, m_aOclDerivedCurrency), 'MultiDerivedCurrency');
      Assert.AreEqual(0, m_aMultiDerivedDate.CompareToAs(ctDefault, m_aOclDerivedDate), 'MultiDerivedDate');
      Assert.AreEqual(0, m_aMultiDerivedDateTime.CompareToAs(ctDefault, m_aOclDerivedDateTime), 'MultiDerivedDateTime');
      Assert.AreEqual(0, m_aMultiDerivedFloat.CompareToAs(ctDefault, m_aOclDerivedFloat), 'MultiDerivedFloat');
      Assert.AreEqual(0, m_aMultiDerivedInteger.CompareToAs(ctDefault, m_aOclDerivedInteger), 'MultiDerivedInteger');
      Assert.AreEqual(0, m_aMultiDerivedShortInt.CompareToAs(ctDefault, m_aOclDerivedShortInt), 'MultiDerivedShortInt');
      Assert.AreEqual(0, m_aMultiDerivedSmallInt.CompareToAs(ctDefault, m_aOclDerivedSmallInt), 'MultiDerivedSmallInt');
      Assert.AreEqual(0, m_aMultiDerivedString.CompareToAs(ctDefault, m_aOclDerivedString), 'MultiDerivedString');
      Assert.AreEqual(0, m_aMultiDerivedTime.CompareToAs(ctDefault, m_aOclDerivedTime), 'MultiDerivedTime');
      Assert.AreEqual(0, m_aMultiDerivedWord.CompareToAs(ctDefault, m_aOclDerivedWord), 'MultiDerivedWord');
    end;
  finally
    ObjectMultiOclSubA.Delete;
  end;
end;

procedure TTestBoldAttributes.CompareToAsTest;
var
  TestClass: TClassB;
begin
  TestClass := TClassB.Create(nil);
  try
    DoTestTime(TestClass.m_aTime, TestClass.m_bTime);
    DoTestString(TestClass.m_aString, TestClass.m_bString);
    DoTestInteger(TestClass.m_aInteger, TestClass.m_bInteger);
    DoTestInteger(TestClass.m_aSmallInt, TestClass.m_bSmallInt);
    DoTestInteger(TestClass.m_aShortInt, TestClass.m_bShortInt);
    DoTestInteger(TestClass.m_aWord, TestClass.m_bWord);
    DoTestInteger(TestClass.m_aByte, TestClass.m_bByte);
    DoTestCurrency(TestClass.m_aCurrency, TestClass.m_bCurrency);
    DoTestDate(TestClass.m_aDate, TestClass.m_bDate);
    DoTestDateTime(TestClass.m_aDateTime, TestClass.m_bDateTime);
    DoTestString(TestClass.m_aBlob, TestClass.m_bBlob);
    DoTestString(TestClass.m_aBlobContent, TestClass.m_bBlobContent);
  finally
    TestClass.Delete;
  end;
end;

procedure TTestBoldAttributes.DoTestString(OperandA, OperandB: TBoldAttribute);
begin
  OperandA.AsString := 'AB';
  OperandB.AsString := 'AC';
  DoTestCompareToAs(OperandA, OperandB, -1);
  OperandB.AsString := 'AA';
  DoTestCompareToAs(OperandA, OperandB, 1);
end;

procedure TTestBoldAttributes.DoTestCompareToAs(Op1, Op2: TBoldAttribute; Expected: Integer);
begin
  Assert.AreEqual(Expected, Op1.CompareToAs(ctDefault, Op2), Format('Comparison %s failed', [Op1.ClassName]));
end;

procedure TTestBoldAttributes.DoTestInteger(OperandA, OperandB: TBAInteger);
begin
  OperandA.AsInteger := 2;
  OperandB.AsInteger := 3;
  DoTestCompareToAs(OperandA, OperandB, -1);
  OperandB.AsInteger := 1;
  DoTestCompareToAs(OperandA, OperandB, 1);
end;

procedure TTestBoldAttributes.DoTestCurrency(OperandA, OperandB: TBACurrency);
begin
  OperandA.AsFloat := 2.2;
  OperandB.AsFloat := 3.3;
  DoTestCompareToAs(OperandA, OperandB, -1);
  OperandB.AsFloat := 1.1;
  DoTestCompareToAs(OperandA, OperandB, 1);
end;

procedure TTestBoldAttributes.DoTestDate(OperandA, OperandB: TBADate);
begin
  OperandA.AsDate := 2;
  OperandB.AsDate := 3;
  DoTestCompareToAs(OperandA, OperandB, -1);
  OperandB.AsDate := 1;
  DoTestCompareToAs(OperandA, OperandB, 1);
end;

procedure TTestBoldAttributes.DoTestFloat(OperandA, OperandB: TBAFloat);
begin
  OperandA.AsFloat := 2;
  OperandB.AsFloat := 3;
  DoTestCompareToAs(OperandA, OperandB, -1);
  OperandB.AsFloat := 1;
  DoTestCompareToAs(OperandA, OperandB, 1);
end;

procedure TTestBoldAttributes.DoTestDateTime(OperandA, OperandB: TBADateTime);
begin
  OperandA.AsDate := 2;
  OperandB.AsDate := 3;
  DoTestCompareToAs(OperandA, OperandB, -1);
  OperandB.AsDate := 1;
  DoTestCompareToAs(OperandA, OperandB, 1);
end;

procedure TTestBoldAttributes.DoTestTime(OperandA, OperandB: TBATime);
begin
  OperandA.AsTime := 2.4;
  OperandB.AsTime := 3.5;
  DoTestCompareToAs(OperandA, OperandB, -1);
  OperandB.AsTime := 1.3;
  DoTestCompareToAs(OperandA, OperandB, 1);
end;

procedure TTestBoldAttributes.CompareToAsNullTest;
var
  ObjectB: TClassB;
  ObjectA: TClassA;
begin
  ObjectB := TClassB.Create(nil);
  ObjectA := TClassA.Create(nil);
  try
    // Set B attributes to null
    with ObjectB do
    begin
      m_bInteger.SetToNull;
      m_bString.SetToNull;
      m_bBoolean.SetToNull;
      m_bByte.SetToNull;
      m_bCurrency.SetToNull;
      m_bTime.SetToNull;
      m_bDate.SetToNull;
      m_bFloat.SetToNull;
      m_bDateTime.SetToNull;
      m_bWord.SetToNull;
      m_bShortInt.SetToNull;
      m_bSmallInt.SetToNull;
      m_bBlob.SetToNull;
      m_bBlobContent.SetToNull;
    end;

    // Set A attributes to values
    with ObjectA do
    begin
      m_aInteger.AsInteger := 2;
      m_aString.AsString := 'AAA';
      m_aBoolean.AsBoolean := True;
      m_aByte.AsByte := 5;
      m_aCurrency.AsCurrency := 23.421;
      m_aTime.AsTime := 1.2;
      m_aDate.AsDate := 2;
      m_aFloat.AsFloat := 3.43;
      m_aDateTime.AsDateTime := 4.2;
      m_aWord.AsWord := 4;
      m_aShortInt.AsShortInt := 24;
      m_aSmallInt.AsSmallInt := 122;
      m_aBlob.AsString := 'yfiew';
      m_aBlobContent.AsString := 'graij';
    end;

    // Compare null vs non-null (null should be less)
    with ObjectB do
    begin
      DoTestCompareToAs(m_bInteger, ObjectA.m_aInteger, -1);
      DoTestCompareToAs(m_bString, ObjectA.m_aString, -1);
      DoTestCompareToAs(m_bBoolean, ObjectA.m_aBoolean, -1);
      DoTestCompareToAs(m_bByte, ObjectA.m_aByte, -1);
      DoTestCompareToAs(m_bCurrency, ObjectA.m_aCurrency, -1);
      DoTestCompareToAs(m_bTime, ObjectA.m_aTime, -1);
      DoTestCompareToAs(m_bDate, ObjectA.m_aDate, -1);
      DoTestCompareToAs(m_bFloat, ObjectA.m_aFloat, -1);
      DoTestCompareToAs(m_bDateTime, ObjectA.m_aDateTime, -1);
      DoTestCompareToAs(m_bWord, ObjectA.m_aWord, -1);
      DoTestCompareToAs(m_bShortInt, ObjectA.m_aShortInt, -1);
      DoTestCompareToAs(m_bSmallInt, ObjectA.m_aSmallInt, -1);
      DoTestCompareToAs(m_bBlob, ObjectA.m_aBlob, -1);
      DoTestCompareToAs(m_bBlobContent, ObjectA.m_aBlobContent, -1);
    end;
  finally
    ObjectB.Delete;
    ObjectA.Delete;
  end;
end;

procedure TTestBoldAttributes.DoCheckCompareTypes(OperandA, OperandB: TBoldAttribute; ValidCompareTypes: TCompareTypeSet);
begin
  InternalCheckCompareTypes(OperandA, OperandB, ctDefault, ctDefault in ValidCompareTypes);
  InternalCheckCompareTypes(OperandA, OperandB, ctCaseSensitive, ctCaseSensitive in ValidCompareTypes);
  InternalCheckCompareTypes(OperandA, OperandB, ctCaseInsensitive, ctCaseInsensitive in ValidCompareTypes);
  InternalCheckCompareTypes(OperandA, OperandB, ctAsDate, ctAsDate in ValidCompareTypes);
  InternalCheckCompareTypes(OperandA, OperandB, ctAsTime, ctAsTime in ValidCompareTypes);
end;

procedure TTestBoldAttributes.TestCompareTypes;
var
  TestClass: TClassA;
begin
  TestClass := TClassA.Create(nil);
  try
    with TestClass do
    begin
      m_aInteger.AsInteger := 2;
      m_aString.AsString := 'AAA';
      m_aBoolean.AsBoolean := True;
      m_aByte.AsByte := 5;
      m_aCurrency.AsCurrency := 23.421;
      m_aTime.AsTime := 1.2;
      m_aDate.AsDate := 2;
      m_aFloat.AsFloat := 3.43;
      m_aDateTime.AsDateTime := 4.2;
      m_aWord.AsWord := 4;
      m_aShortInt.AsShortInt := 24;
      m_aSmallInt.AsSmallInt := 122;
      m_aBlob.AsString := 'yfiew';
      m_aBlobContent.AsString := 'graij';
    end;

    with TestClass do
    begin
      DoCheckCompareTypes(m_aInteger, m_aInteger, [ctDefault]);
      DoCheckCompareTypes(m_aString, m_aString, [ctDefault, ctCaseSensitive, ctCaseInsensitive]);
      DoCheckCompareTypes(m_aBoolean, m_aBoolean, [ctDefault]);
      DoCheckCompareTypes(m_aByte, m_aByte, [ctDefault]);
      DoCheckCompareTypes(m_aCurrency, m_aCurrency, [ctDefault]);
      DoCheckCompareTypes(m_aTime, m_aTime, [ctDefault, ctAsTime]);
      DoCheckCompareTypes(m_aDate, m_aDate, [ctDefault, ctAsDate]);
      DoCheckCompareTypes(m_aFloat, m_aFloat, [ctDefault]);
      DoCheckCompareTypes(m_aDateTime, m_aDateTime, [ctDefault, ctAsDate, ctAsTime]);
      DoCheckCompareTypes(m_aWord, m_aWord, [ctDefault]);
      DoCheckCompareTypes(m_aShortInt, m_aShortInt, [ctDefault]);
      DoCheckCompareTypes(m_aSmallInt, m_aSmallInt, [ctDefault]);
      DoCheckCompareTypes(m_aBlob, m_aBlob, [ctDefault, ctCaseSensitive, ctCaseInsensitive]);
      DoCheckCompareTypes(m_aBlobContent, m_aBlobContent, [ctDefault, ctCaseSensitive, ctCaseInsensitive]);
    end;
  finally
    TestClass.Delete;
  end;
end;

procedure TTestBoldAttributes.InternalCheckCompareTypes(OperandA, OperandB: TBoldAttribute;
  CompareType: TBoldCompareType; WillWork: Boolean);
begin
  try
    if WillWork then
      OperandA.CompareToAs(CompareType, OperandB);
  except
    Assert.IsFalse(WillWork, Format('CompareType %d should not work', [Ord(CompareType)]));
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldAttributes);

end.
