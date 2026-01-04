unit Test.BoldAttributes;

{ DUnitX version of dmjehoBoldTest - Bold Attribute Tests }

interface

uses
  Classes,
  Forms,
  DUnitX.TestFramework,
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
  BoldAbstractModel,
  BoldListHandle;

type
  TCompareTypeSet = set of TBoldCompareType;

  TjehodmBoldTest = class(TDataModule)
    BoldModel1: TBoldModel;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldTypeNameHandle1: TBoldTypeNameHandle;
    BoldListHandle1: TBoldListHandle;
  end;

  [TestFixture]
  [Category('ObjectSpace')]
  TTestBoldAttributes = class
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
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    [Test]
    [Category('Quick')]
    procedure AttributeValueTest;
    [Test]
    [Category('Quick')]
    procedure AttributeNullTest;
    [Test]
    [Category('Quick')]
    procedure AttributeIndexTest;
    [Test]
    [Category('Quick')]
    procedure AttributeDerivationTest;
    [Test]
    [Category('Quick')]
    procedure AttributeOclDerivationTest;
    [Test]
    [Category('Quick')]
    procedure CheckMultiDerivedAttributes;
    [Test]
    [Category('Quick')]
    procedure CompareToAsTest;
    [Test]
    [Category('Quick')]
    procedure CompareToAsNullTest;
    [Test]
    [Category('Quick')]
    procedure TestCompareTypes;
    [Test]
    [Category('Quick')]
    procedure TestBlobContentTypeValidation;
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
  ObjectClassA.aInteger := INTEGERCONST;
  ObjectClassA.aFloat := FLOATCONST;
  ObjectClassA.aSmallInt := INTEGERCONST;
  ObjectClassA.aCurrency := CURRENCYCONST;
  ObjectClassA.aTime := FLOATCONST;
  ObjectClassA.aWord := INTEGERCONST;
  ObjectClassA.aString := STRINGCONST;
  ObjectClassA.aDateTime := FLOATCONST;
  ObjectClassA.aByte := INTEGERCONST;
  ObjectClassA.aDate := INTEGERCONST;
  ObjectClassA.aBoolean := BOOLEANCONST;
  ObjectClassA.aShortInt := INTEGERCONST;
  ObjectClassA.aBlob := STRINGCONST;
  ObjectClassA.aBlobContent := STRINGCONST;
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
  ObjectClassA.m_aInteger.SetToNull;
  ObjectClassA.m_aString.SetToNull;
  ObjectClassA.m_aBoolean.SetToNull;
  ObjectClassA.m_aByte.SetToNull;
  ObjectClassA.m_aCurrency.SetToNull;
  ObjectClassA.m_aTime.SetToNull;
  ObjectClassA.m_aDate.SetToNull;
  ObjectClassA.m_aFloat.SetToNull;
  ObjectClassA.m_aDateTime.SetToNull;
  ObjectClassA.m_aWord.SetToNull;
  ObjectClassA.m_aShortInt.SetToNull;
  ObjectClassA.m_aSmallInt.SetToNull;
  ObjectClassA.m_aBlob.SetToNull;
  ObjectClassA.m_aBlobContent.SetToNull;
end;

procedure TTestBoldAttributes.CheckNullAttributes(ObjectClassA: TClassA);
begin
  Assert.IsTrue(ObjectClassA.m_aInteger.IsNull, 'm_aInteger');
  Assert.IsTrue(ObjectClassA.m_aFloat.IsNull, 'm_aFloat');
  Assert.IsTrue(ObjectClassA.m_aSmallInt.IsNull, 'm_aSmallInt');
  Assert.IsTrue(ObjectClassA.m_aCurrency.IsNull, 'm_aCurrency');
  Assert.IsTrue(ObjectClassA.m_aTime.IsNull, 'm_aTime');
  Assert.IsTrue(ObjectClassA.m_aWord.IsNull, 'm_aWord');
  Assert.IsTrue(ObjectClassA.m_aString.IsNull, 'm_aString');
  Assert.IsTrue(ObjectClassA.m_aDateTime.IsNull, 'm_aDateTime');
  Assert.IsTrue(ObjectClassA.m_aByte.IsNull, 'm_aByte');
  Assert.IsTrue(ObjectClassA.m_aDate.IsNull, 'm_aDate');
  Assert.IsTrue(ObjectClassA.m_aBoolean.IsNull, 'm_aBoolean');
  Assert.IsTrue(ObjectClassA.m_aShortInt.IsNull, 'm_aShortInt');
  Assert.IsTrue(ObjectClassA.m_aBlob.IsNull, 'm_aBlob');
  Assert.IsTrue(ObjectClassA.m_aBlobContent.IsNull, 'm_aBlobContent');
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

  AccessMember(ObjectClassA.m_aString);
  AccessMember(ObjectClassA.m_aBoolean);
  AccessMember(ObjectClassA.m_aByte);
  AccessMember(ObjectClassA.m_aCurrency);
  AccessMember(ObjectClassA.m_aDate);
  AccessMember(ObjectClassA.m_aDateTime);
  AccessMember(ObjectClassA.m_aFloat);
  AccessMember(ObjectClassA.m_aInteger);
  AccessMember(ObjectClassA.m_aShortInt);
  AccessMember(ObjectClassA.m_aSmallInt);
  AccessMember(ObjectClassA.m_aTime);
  AccessMember(ObjectClassA.m_aWord);
  AccessMember(ObjectClassA.m_aBlob);
  AccessMember(ObjectClassA.m_aBlobContent);
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
  CompareMembers(ObjectSubA.m_aDerivedInteger, ObjectSubA.m_aInteger);
  CompareMembers(ObjectSubA.m_aDerivedFloat, ObjectSubA.m_aFloat);
  CompareMembers(ObjectSubA.m_aDerivedSmallInt, ObjectSubA.m_aSmallInt);
  CompareMembers(ObjectSubA.m_aDerivedCurrency, ObjectSubA.m_aCurrency);
  CompareMembers(ObjectSubA.m_aDerivedTime, ObjectSubA.m_aTime);
  CompareMembers(ObjectSubA.m_aDerivedWord, ObjectSubA.m_aWord);
  CompareMembers(ObjectSubA.m_aDerivedString, ObjectSubA.m_aString);
  CompareMembers(ObjectSubA.m_aDerivedDateTime, ObjectSubA.m_aDateTime);
  CompareMembers(ObjectSubA.m_aDerivedByte, ObjectSubA.m_aByte);
  CompareMembers(ObjectSubA.m_aDerivedDate, ObjectSubA.m_aDate);
  CompareMembers(ObjectSubA.m_aDerivedBoolean, ObjectSubA.m_aBoolean);
  CompareMembers(ObjectSubA.m_aDerivedBlob, ObjectSubA.m_aBlob);
  CompareMembers(ObjectSubA.m_aDerivedBlobContent, ObjectSubA.m_aBlobContent);
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
  CompareMembers(ObjectOclSubA.m_aOclDerivedInteger, ObjectOclSubA.m_aInteger);
  CompareMembers(ObjectOclSubA.m_aOclDerivedFloat, ObjectOclSubA.m_aFloat);
  CompareMembers(ObjectOclSubA.m_aOclDerivedSmallInt, ObjectOclSubA.m_aSmallInt);
  CompareMembers(ObjectOclSubA.m_aOclDerivedCurrency, ObjectOclSubA.m_aCurrency);
  CompareMembers(ObjectOclSubA.m_aOclDerivedTime, ObjectOclSubA.m_aTime);
  CompareMembers(ObjectOclSubA.m_aOclDerivedWord, ObjectOclSubA.m_aWord);
  CompareMembers(ObjectOclSubA.m_aOclDerivedString, ObjectOclSubA.m_aString);
  CompareMembers(ObjectOclSubA.m_aOclDerivedDateTime, ObjectOclSubA.m_aDateTime);
  CompareMembers(ObjectOclSubA.m_aOclDerivedByte, ObjectOclSubA.m_aByte);
  CompareMembers(ObjectOclSubA.m_aOclDerivedDate, ObjectOclSubA.m_aDate);
  CompareMembers(ObjectOclSubA.m_aOclDerivedBoolean, ObjectOclSubA.m_aBoolean);
  CompareMembers(ObjectOclSubA.m_aOclDerivedBlob, ObjectOclSubA.m_aBlob);
  CompareMembers(ObjectOclSubA.m_aOclDerivedBlobContent, ObjectOclSubA.m_aBlobContent);
end;

procedure TTestBoldAttributes.CheckMultiDerivedAttributes;
var
  Obj: TClassMultiDerivedA;
begin
  Obj := TClassMultiDerivedA.Create(nil);
  try
    SetAttributeValues(Obj);
    Assert.AreEqual(0, Obj.m_aMultiDerivedBoolean.CompareToAs(ctDefault, Obj.m_aOclDerivedBoolean), 'MultiDerivedBoolean');
    Assert.AreEqual(0, Obj.m_aMultiDerivedByte.CompareToAs(ctDefault, Obj.m_aOclDerivedByte), 'MultiDerivedByte');
    Assert.AreEqual(0, Obj.m_aMultiDerivedCurrency.CompareToAs(ctDefault, Obj.m_aOclDerivedCurrency), 'MultiDerivedCurrency');
    Assert.AreEqual(0, Obj.m_aMultiDerivedDate.CompareToAs(ctDefault, Obj.m_aOclDerivedDate), 'MultiDerivedDate');
    Assert.AreEqual(0, Obj.m_aMultiDerivedDateTime.CompareToAs(ctDefault, Obj.m_aOclDerivedDateTime), 'MultiDerivedDateTime');
    Assert.AreEqual(0, Obj.m_aMultiDerivedFloat.CompareToAs(ctDefault, Obj.m_aOclDerivedFloat), 'MultiDerivedFloat');
    Assert.AreEqual(0, Obj.m_aMultiDerivedInteger.CompareToAs(ctDefault, Obj.m_aOclDerivedInteger), 'MultiDerivedInteger');
    Assert.AreEqual(0, Obj.m_aMultiDerivedShortInt.CompareToAs(ctDefault, Obj.m_aOclDerivedShortInt), 'MultiDerivedShortInt');
    Assert.AreEqual(0, Obj.m_aMultiDerivedSmallInt.CompareToAs(ctDefault, Obj.m_aOclDerivedSmallInt), 'MultiDerivedSmallInt');
    Assert.AreEqual(0, Obj.m_aMultiDerivedString.CompareToAs(ctDefault, Obj.m_aOclDerivedString), 'MultiDerivedString');
    Assert.AreEqual(0, Obj.m_aMultiDerivedTime.CompareToAs(ctDefault, Obj.m_aOclDerivedTime), 'MultiDerivedTime');
    Assert.AreEqual(0, Obj.m_aMultiDerivedWord.CompareToAs(ctDefault, Obj.m_aOclDerivedWord), 'MultiDerivedWord');
  finally
    Obj.Delete;
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
    ObjectB.m_bInteger.SetToNull;
    ObjectB.m_bString.SetToNull;
    ObjectB.m_bBoolean.SetToNull;
    ObjectB.m_bByte.SetToNull;
    ObjectB.m_bCurrency.SetToNull;
    ObjectB.m_bTime.SetToNull;
    ObjectB.m_bDate.SetToNull;
    ObjectB.m_bFloat.SetToNull;
    ObjectB.m_bDateTime.SetToNull;
    ObjectB.m_bWord.SetToNull;
    ObjectB.m_bShortInt.SetToNull;
    ObjectB.m_bSmallInt.SetToNull;
    ObjectB.m_bBlob.SetToNull;
    ObjectB.m_bBlobContent.SetToNull;

    // Set A attributes to values
    ObjectA.m_aInteger.AsInteger := 2;
    ObjectA.m_aString.AsString := 'AAA';
    ObjectA.m_aBoolean.AsBoolean := True;
    ObjectA.m_aByte.AsByte := 5;
    ObjectA.m_aCurrency.AsCurrency := 23.421;
    ObjectA.m_aTime.AsTime := 1.2;
    ObjectA.m_aDate.AsDate := 2;
    ObjectA.m_aFloat.AsFloat := 3.43;
    ObjectA.m_aDateTime.AsDateTime := 4.2;
    ObjectA.m_aWord.AsWord := 4;
    ObjectA.m_aShortInt.AsShortInt := 24;
    ObjectA.m_aSmallInt.AsSmallInt := 122;
    ObjectA.m_aBlob.AsString := 'yfiew';
    ObjectA.m_aBlobContent.AsString := 'graij';

    // Compare null vs non-null (null should be less)
    DoTestCompareToAs(ObjectB.m_bInteger, ObjectA.m_aInteger, -1);
    DoTestCompareToAs(ObjectB.m_bString, ObjectA.m_aString, -1);
    DoTestCompareToAs(ObjectB.m_bBoolean, ObjectA.m_aBoolean, -1);
    DoTestCompareToAs(ObjectB.m_bByte, ObjectA.m_aByte, -1);
    DoTestCompareToAs(ObjectB.m_bCurrency, ObjectA.m_aCurrency, -1);
    DoTestCompareToAs(ObjectB.m_bTime, ObjectA.m_aTime, -1);
    DoTestCompareToAs(ObjectB.m_bDate, ObjectA.m_aDate, -1);
    DoTestCompareToAs(ObjectB.m_bFloat, ObjectA.m_aFloat, -1);
    DoTestCompareToAs(ObjectB.m_bDateTime, ObjectA.m_aDateTime, -1);
    DoTestCompareToAs(ObjectB.m_bWord, ObjectA.m_aWord, -1);
    DoTestCompareToAs(ObjectB.m_bShortInt, ObjectA.m_aShortInt, -1);
    DoTestCompareToAs(ObjectB.m_bSmallInt, ObjectA.m_aSmallInt, -1);
    DoTestCompareToAs(ObjectB.m_bBlob, ObjectA.m_aBlob, -1);
    DoTestCompareToAs(ObjectB.m_bBlobContent, ObjectA.m_aBlobContent, -1);
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
    // Set attribute values
    TestClass.m_aInteger.AsInteger := 2;
    TestClass.m_aString.AsString := 'AAA';
    TestClass.m_aBoolean.AsBoolean := True;
    TestClass.m_aByte.AsByte := 5;
    TestClass.m_aCurrency.AsCurrency := 23.421;
    TestClass.m_aTime.AsTime := 1.2;
    TestClass.m_aDate.AsDate := 2;
    TestClass.m_aFloat.AsFloat := 3.43;
    TestClass.m_aDateTime.AsDateTime := 4.2;
    TestClass.m_aWord.AsWord := 4;
    TestClass.m_aShortInt.AsShortInt := 24;
    TestClass.m_aSmallInt.AsSmallInt := 122;
    TestClass.m_aBlob.AsString := 'yfiew';
    TestClass.m_aBlobContent.AsString := 'graij';

    // Check compare types for each attribute
    DoCheckCompareTypes(TestClass.m_aInteger, TestClass.m_aInteger, [ctDefault]);
    DoCheckCompareTypes(TestClass.m_aString, TestClass.m_aString, [ctDefault, ctCaseSensitive, ctCaseInsensitive]);
    DoCheckCompareTypes(TestClass.m_aBoolean, TestClass.m_aBoolean, [ctDefault]);
    DoCheckCompareTypes(TestClass.m_aByte, TestClass.m_aByte, [ctDefault]);
    DoCheckCompareTypes(TestClass.m_aCurrency, TestClass.m_aCurrency, [ctDefault]);
    DoCheckCompareTypes(TestClass.m_aTime, TestClass.m_aTime, [ctDefault, ctAsDate, ctAsTime]);
    DoCheckCompareTypes(TestClass.m_aDate, TestClass.m_aDate, [ctDefault, ctAsDate, ctAsTime]);
    DoCheckCompareTypes(TestClass.m_aFloat, TestClass.m_aFloat, [ctDefault]);
    DoCheckCompareTypes(TestClass.m_aDateTime, TestClass.m_aDateTime, [ctDefault, ctAsDate, ctAsTime]);
    DoCheckCompareTypes(TestClass.m_aWord, TestClass.m_aWord, [ctDefault]);
    DoCheckCompareTypes(TestClass.m_aShortInt, TestClass.m_aShortInt, [ctDefault]);
    DoCheckCompareTypes(TestClass.m_aSmallInt, TestClass.m_aSmallInt, [ctDefault]);
    DoCheckCompareTypes(TestClass.m_aBlob, TestClass.m_aBlob, [ctDefault, ctCaseSensitive, ctCaseInsensitive]);
    DoCheckCompareTypes(TestClass.m_aBlobContent, TestClass.m_aBlobContent, [ctDefault, ctCaseSensitive, ctCaseInsensitive]);
  finally
    TestClass.Delete;
  end;
end;

procedure TTestBoldAttributes.InternalCheckCompareTypes(OperandA, OperandB: TBoldAttribute;
  CompareType: TBoldCompareType; WillWork: Boolean);
var
  ExceptionRaised: Boolean;
begin
  ExceptionRaised := False;
  try
    OperandA.CompareToAs(CompareType, OperandB);
  except
    ExceptionRaised := True;
  end;
  if WillWork then
    Assert.IsFalse(ExceptionRaised, Format('CompareType %d should work but raised exception', [Ord(CompareType)]))
  else
    Assert.IsTrue(ExceptionRaised, Format('CompareType %d should not work but no exception', [Ord(CompareType)]));
end;

procedure TTestBoldAttributes.TestBlobContentTypeValidation;
var
  BlobJPEG: TBABlobImageJPEG;
  BlobBMP: TBABlobImageBMP;
  TypedBlob: TBATypedBlob;
  ExceptionRaised: Boolean;
begin
  // Test TBABlobImageJPEG - should reject wrong content type
  BlobJPEG := TBABlobImageJPEG.Create;
  try
    // Setting correct content type should work (no exception)
    ExceptionRaised := False;
    try
      BlobJPEG.ContentType := 'image/jpeg';
    except
      ExceptionRaised := True;
    end;
    Assert.IsFalse(ExceptionRaised, 'JPEG blob should accept image/jpeg content type');

    // Setting wrong content type should raise exception
    ExceptionRaised := False;
    try
      BlobJPEG.ContentType := 'image/png';
    except
      ExceptionRaised := True;
    end;
    Assert.IsTrue(ExceptionRaised, 'JPEG blob should reject image/png content type');
  finally
    BlobJPEG.Free;
  end;

  // Test TBABlobImageBMP - should reject wrong content type
  BlobBMP := TBABlobImageBMP.Create;
  try
    // Setting correct content type should work
    ExceptionRaised := False;
    try
      BlobBMP.ContentType := 'image/bitmap';
    except
      ExceptionRaised := True;
    end;
    Assert.IsFalse(ExceptionRaised, 'BMP blob should accept image/bitmap content type');

    // Setting wrong content type should raise exception
    ExceptionRaised := False;
    try
      BlobBMP.ContentType := 'image/jpeg';
    except
      ExceptionRaised := True;
    end;
    Assert.IsTrue(ExceptionRaised, 'BMP blob should reject image/jpeg content type');
  finally
    BlobBMP.Free;
  end;

  // Test TBATypedBlob - should allow changing content type
  TypedBlob := TBATypedBlob.Create;
  try
    ExceptionRaised := False;
    try
      TypedBlob.ContentType := 'application/pdf';
    except
      ExceptionRaised := True;
    end;
    Assert.IsFalse(ExceptionRaised, 'TypedBlob should accept any content type');
    Assert.AreEqual('application/pdf', TypedBlob.ContentType, 'TypedBlob content type should be set');

    // Change to different type should also work
    ExceptionRaised := False;
    try
      TypedBlob.ContentType := 'text/plain';
    except
      ExceptionRaised := True;
    end;
    Assert.IsFalse(ExceptionRaised, 'TypedBlob should allow changing content type');
    Assert.AreEqual('text/plain', TypedBlob.ContentType, 'TypedBlob content type should be changed');
  finally
    TypedBlob.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldAttributes);

end.
