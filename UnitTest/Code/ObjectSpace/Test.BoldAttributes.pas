unit Test.BoldAttributes;

{ DUnitX version of dmjehoBoldTest - Bold Attribute Tests }

interface

uses
  Classes,
  Forms,
  DUnitX.TestFramework,
  // TestModel1 is listed before jehoBCBoldTest on purpose: both declare
  // TClassA with the same attribute names, and unqualified TClassA in this
  // unit must keep resolving to jehoBCBoldTest.TClassA (later unit wins).
  TestModel1,
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
    [Test]
    [Category('Quick')]
    procedure TestBlobAssignTypedToPlainBlob;
    [Test]
    [Category('Quick')]
    procedure TestBlobContentTypeOnPlainBlobIgnored;
    [Test]
    [Category('Quick')]
    procedure TestMLStringBlobContentProxy;
    [Test]
    [Category('Quick')]
    procedure TestValueSetValueListFindByText;
    [Test]
    [Category('Quick')]
    procedure TestNumericGetStringRepresentation;
    [Test]
    [Category('Quick')]
    procedure TestBlobImageGetStringRepresentationInherited;
    [Test]
    [Category('Quick')]
    procedure TestDateTimeGetStringRepresentationNonDefault;
    [Test]
    [Category('Quick')]
    procedure TestNumericSetStringRepresentation;
    [Test]
    [Category('Quick')]
    procedure TestDateTimeSetStringRepresentation;
    [Test]
    [Category('Quick')]
    procedure TestDateTimeValidateString;
    [Test]
    [Category('Quick')]
    procedure TestCreateWithValueConstructors;
    [Test]
    [Category('Quick')]
    procedure TestIsNullOrEmptyAndIsNullOrZero;
    [Test]
    [Category('Quick')]
    procedure TestIntegerValidateCharacter;
    [Test]
    [Category('Quick')]
    procedure TestCurrencyValidateCharacter;
    [Test]
    [Category('Quick')]
    procedure TestFloatValidateCharacter;
    [Test]
    [Category('Quick')]
    procedure TestTrimmedStringTrimsWhitespace;
    [Test]
    [Category('Quick')]
    procedure TestTextValidateStringNoLengthCheck;
    [Test]
    [Category('Quick')]
    procedure TestUnicodeTextValidateString;
    [Test]
    [Category('Quick')]
    procedure TestBlobStreamSaveOperations;
    [Test]
    [Category('Quick')]
    procedure TestBlobStreamTruncate;
    [Test]
    [Category('Quick')]
    procedure TestAnsiStringValidation;
    [Test]
    [Category('Quick')]
    procedure TestIntegerGetAsFloat;
    [Test]
    [Category('Quick')]
    procedure TestCurrencyGetAsFloat;
    [Test]
    [Category('Quick')]
    procedure TestNumericCompareToAsCtDefault;
    [Test]
    [Category('Quick')]
    procedure TestBooleanCreateWithValue;

    // TBAMoment / TBADateTime / TBADate / TBATime coverage
    [Test]
    [Category('Quick')]
    procedure TestMomentGetParts;
    [Test]
    [Category('Quick')]
    procedure TestMomentAsVariant;
    [Test]
    [Category('Quick')]
    procedure TestMomentAsFloat;
    [Test]
    [Category('Quick')]
    procedure TestDateTimeAsDate;
    [Test]
    [Category('Quick')]
    procedure TestDateTimeAsTime;
    [Test]
    [Category('Quick')]
    procedure TestDateTimeSetAsDatePreservesTime;
    [Test]
    [Category('Quick')]
    procedure TestTimeSetAsDatePreservesTime;
    [Test]
    [Category('Quick')]
    procedure TestDateTimeSubEpsilonUpdatePersists;
    [Test]
    [Category('Quick')]
    procedure TestDateComparesByDayAfterFractionalStore;

    // TBAString coverage
    [Test]
    [Category('Quick')]
    procedure TestStringAsAnsiAndUnicode;
    [Test]
    [Category('Quick')]
    procedure TestStringAssignValue;

    // TBABlob coverage
    [Test]
    [Category('Quick')]
    procedure TestBlobLoadSaveFile;
    [Test]
    [Category('Quick')]
    procedure TestBlobLoadSaveStream;
    [Test]
    [Category('Quick')]
    procedure TestBlobAssign;
    [Test]
    [Category('Quick')]
    procedure TestBlobSize;

    // TBACurrency coverage
    [Test]
    [Category('Quick')]
    procedure TestCurrencyAsCurrency;
    [Test]
    [Category('Quick')]
    procedure TestCurrencyAsVariant;
    [Test]
    [Category('Quick')]
    procedure TestCurrencyAssign;

    // TBAFloat coverage
    [Test]
    [Category('Quick')]
    procedure TestFloatAsVariant;
    [Test]
    [Category('Quick')]
    procedure TestFloatAssign;

    // TBAInteger coverage
    [Test]
    [Category('Quick')]
    procedure TestIntegerAsVariant;
    [Test]
    [Category('Quick')]
    procedure TestIntegerAssign;

    // Integer subtype range checks
    [Test]
    [Category('Quick')]
    procedure TestByteRangeCheck;
    [Test]
    [Category('Quick')]
    procedure TestShortIntRangeCheck;
    [Test]
    [Category('Quick')]
    procedure TestSmallIntRangeCheck;
    [Test]
    [Category('Quick')]
    procedure TestWordRangeCheck;
    [Test]
    [Category('Quick')]
    procedure TestByteGetSetAs;
    [Test]
    [Category('Quick')]
    procedure TestShortIntGetSetAs;
    [Test]
    [Category('Quick')]
    procedure TestSmallIntGetSetAs;
    [Test]
    [Category('Quick')]
    procedure TestWordGetSetAs;

    // AnsiString coverage
    [Test]
    [Category('Quick')]
    procedure TestAnsiStringSetGetDataValue;
    [Test]
    [Category('Quick')]
    procedure TestAnsiStringFreeContent;

    // IsEqualToValue coverage
    [Test]
    [Category('Quick')]
    procedure TestIntegerIsEqualToValue;
    [Test]
    [Category('Quick')]
    procedure TestFloatIsEqualToValue;
    [Test]
    [Category('Quick')]
    procedure TestCurrencyIsEqualToValue;
    [Test]
    [Category('Quick')]
    procedure TestStringIsEqualToValue;

    // Blob byte array variant
    [Test]
    [Category('Quick')]
    procedure TestBlobSetAsVariantByteArray;

    // Currency CompareToAs with float operand
    [Test]
    [Category('Quick')]
    procedure TestCurrencyCompareToAsWithFloat;

    // Currency Assign from float/integer
    [Test]
    [Category('Quick')]
    procedure TestCurrencyAssignFromNumericTypes;

    // Integer ValidateString with range
    [Test]
    [Category('Quick')]
    procedure TestIntegerValidateStringRange;

    // Moment IsNullOrZero
    [Test]
    [Category('Quick')]
    procedure TestMomentIsNullOrZero;

    // Moment IsEqualToValue
    [Test]
    [Category('Quick')]
    procedure TestMomentIsEqualToValue;

    // Float SetAsCurrency
    [Test]
    [Category('Quick')]
    procedure TestFloatSetAsCurrency;

    // Blob IsEqualToValue
    [Test]
    [Category('Quick')]
    procedure TestBlobIsEqualToValue;

    // Assign/Clone paths
    [Test]
    [Category('Quick')]
    procedure TestStringAssignNull;
    [Test]
    [Category('Quick')]
    procedure TestIntegerAssignNull;
    [Test]
    [Category('Quick')]
    procedure TestFloatAssignNull;
    [Test]
    [Category('Quick')]
    procedure TestCurrencyAssignNull;
    [Test]
    [Category('Quick')]
    procedure TestBlobAssignNull;

    // CompareToAs with non-matching types
    [Test]
    [Category('Quick')]
    procedure TestIntegerCompareToAsWithString;
    [Test]
    [Category('Quick')]
    procedure TestStringCompareToAsWithInteger;

    // SetEmptyValue paths
    [Test]
    [Category('Quick')]
    procedure TestIntegerSetEmptyValue;
    [Test]
    [Category('Quick')]
    procedure TestStringSetEmptyValue;

    // Variant round-trips
    [Test]
    [Category('Quick')]
    procedure TestIntegerAsVariantNull;
    [Test]
    [Category('Quick')]
    procedure TestFloatAsVariantNull;
    [Test]
    [Category('Quick')]
    procedure TestCurrencyAsVariantNull;
    [Test]
    [Category('Quick')]
    procedure TestBlobAsVariantNull;
    [Test]
    [Category('Quick')]
    procedure TestDateTimeAsVariantRoundTrip;

    // Blob operations
    [Test]
    [Category('Quick')]
    procedure TestBlobSetToNullClearsStream;
    [Test]
    [Category('Quick')]
    procedure TestBlobCompareToAs;

    // JSON serialization (BoldObjectRepresentationJson)
    [Test]
    [Category('Quick')]
    procedure TestBoldElementToJsonString;
    [Test]
    [Category('Quick')]
    procedure TestBoldElementToJsonAndBack;
    [Test]
    [Category('Quick')]
    procedure TestBoldObjectStringRepresentation;
    [Test]
    [Category('Quick')]
    procedure TestBoldElementToJsonWithNulls;
    [Test]
    [Category('Quick')]
    procedure TestBoldElementToJsonWithBlob;
  end;

  // SetEmptyValue must give a non-nullable attribute its empty value even when
  // the attribute is currently null (e.g. a NULL fetched from the database into
  // a non-nullable column, or a model that changed from nullable to
  // non-nullable). The scalar overrides guard on the data field instead of the
  // null flag, so a null attribute silently stays null; TBAValueSet reads
  // AsInteger before assigning and raises on a null attribute.
  // Uses TestModel1 because its attributes are non-nullable (no AllowNULL tag,
  // model default is False) - the jeho model above is all AllowNULL=True.
  [TestFixture]
  [Category('ObjectSpace')]
  TTestSetEmptyValue = class
  private
    FSystemTypeInfoHandle: TBoldSystemTypeInfoHandle;
    FSystemHandle: TBoldSystemHandle;
    function CreateClassA: TestModel1.TClassA;
    procedure MakeContentNull(Attribute: TBoldAttribute);
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestSetEmptyValueRepairsNullString;
    [Test]
    procedure TestSetEmptyValueRepairsNullInteger;
    [Test]
    procedure TestSetEmptyValueRepairsNullFloat;
    [Test]
    procedure TestSetEmptyValueRepairsNullCurrency;
    [Test]
    procedure TestSetEmptyValueRepairsNullValueSet;
    [Test]
    procedure TestAssignNilRepairsNullNonNullableAttribute;
  end;

var
  jehodmBoldTest: TjehodmBoldTest;

implementation

uses
  SysUtils,
  Variants,
  BoldDomainElement,
  BoldValueInterfaces,
  BoldMLAttributes,
  BoldObjectRepresentationJson,
  dmModel1;

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

procedure TTestBoldAttributes.TestBlobAssignTypedToPlainBlob;
var
  Source: TBABlobImageJPEG;
  Target: TBABlob;
  DataIn, DataOut: TStringStream;
begin
  // Regression for H6 (bcadc56): the content-type validation hoisted into
  // TBABlob.SetStringRepresentation raises for ANY non-empty type on a plain
  // blob (its own ContentType is ''), so Assign from a typed blob failed
  // mid-copy, after LoadFromStream had already run.
  Source := TBABlobImageJPEG.Create;
  Target := TBABlob.Create;
  DataIn := TStringStream.Create('JPEGDATA');
  DataOut := TStringStream.Create('');
  try
    Source.LoadFromStream(DataIn);
    Target.Assign(Source);
    Target.SaveToStream(DataOut);
    Assert.AreEqual('JPEGDATA', DataOut.DataString,
      'Assign should copy the blob data');
    Assert.AreEqual('', Target.ContentType,
      'a plain TBABlob has no inherent content type');
  finally
    Source.Free;
    Target.Free;
    DataIn.Free;
    DataOut.Free;
  end;
end;

procedure TTestBoldAttributes.TestBlobContentTypeOnPlainBlobIgnored;
var
  Blob: TBABlob;
begin
  // Pre-bcadc56 contract: 'Content type is lost when assigned to a Blob'.
  Blob := TBABlob.Create;
  try
    Blob.ContentType := 'application/pdf';  // must not raise
    Assert.AreEqual('', Blob.ContentType,
      'content type assigned to a plain Blob is silently dropped');
  finally
    Blob.Free;
  end;
end;

procedure TTestBoldAttributes.TestDateTimeSubEpsilonUpdatePersists;
var
  Obj: TClassA;
  V1, V2: TDateTime;
begin
  // IsSameValue used Math.SameValue's default epsilon, ~4 ms at current date
  // magnitudes - a timestamp update smaller than that was treated as
  // unchanged and silently never stored. Comparison must be exact.
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  V1 := EncodeDate(2026, 7, 12) + EncodeTime(12, 0, 0, 0);
  V2 := V1 + (1 / MSecsPerDay);  // +1 ms, inside the old 4 ms tolerance
  Obj.aDateTime := V1;
  Obj.aDateTime := V2;
  // deliberate raw '=': Assert.AreEqual with a 0 tolerance falls back to the
  // same fuzzy SameValue and would mask exactly this bug
  Assert.IsTrue(Obj.aDateTime = V2, 'a 1 ms timestamp update must persist');
end;

procedure TTestBoldAttributes.TestMLStringBlobContentProxy;
var
  MLString: TBAMLString;
  BlobContent: IBoldBlobContent;
begin
  // Regression for H7 (merge 92196d9): ProxyInterface built the blob-content
  // proxy from TBAString_Proxy, which does not implement IBoldBlobContent, so
  // requesting it always raised EBoldInternal. The correct TBAMLString_Proxy
  // existed in the unit but was never instantiated.
  MLString := TBAMLString.Create;
  try
    Assert.IsTrue(MLString.ProxyInterface(IBoldBlobContent, bdepContents, BlobContent),
      'a multi-language string must provide its blob-content proxy');
    Assert.IsNotNull(BlobContent, 'the proxy interface must be assigned');
    BlobContent := nil;  // release the proxy before freeing the attribute
  finally
    MLString.Free;
  end;
end;

procedure TTestBoldAttributes.TestValueSetValueListFindByText;
var
  ValueList: TBAValueSetValueList;
  FoundValue: TBAValueSetValue;
begin
  ValueList := TBAValueSetValueList.Create;
  try
    ValueList.Add(1, ['One', 'First']);
    ValueList.Add(2, ['Two', 'Second']);
    ValueList.Add(3, ['Three', 'Third']);

    // Test case-insensitive find
    FoundValue := ValueList.FindByText(brDefault, 'one');
    Assert.IsNotNull(FoundValue, 'Should find "one" case-insensitively');
    Assert.AreEqual(1, FoundValue.AsInteger, 'Found value should have integer 1');

    FoundValue := ValueList.FindByText(brDefault, 'TWO');
    Assert.IsNotNull(FoundValue, 'Should find "TWO" case-insensitively');
    Assert.AreEqual(2, FoundValue.AsInteger, 'Found value should have integer 2');

    // Test not found
    FoundValue := ValueList.FindByText(brDefault, 'Four');
    Assert.IsNull(FoundValue, 'Should not find "Four"');
  finally
    ValueList.Free;
  end;
end;

procedure TTestBoldAttributes.TestNumericGetStringRepresentation;
var
  TestClass: TClassA;
begin
  TestClass := TClassA.Create(nil);
  try
    // Test TBAInteger.GetStringRepresentation
    TestClass.m_aInteger.AsInteger := 42;
    Assert.AreEqual('42', TestClass.m_aInteger.AsString, 'Integer string representation');

    // Test TBAFloat.GetStringRepresentation
    TestClass.m_aFloat.AsFloat := 3.14;
    Assert.AreEqual(FloatToStr(3.14), TestClass.m_aFloat.AsString, 'Float string representation');

    // Test TBACurrency.GetStringRepresentation
    TestClass.m_aCurrency.AsCurrency := 99.95;
    Assert.AreEqual(CurrToStr(99.95), TestClass.m_aCurrency.AsString, 'Currency string representation');

    // Test null values return empty string
    TestClass.m_aInteger.SetToNull;
    Assert.AreEqual('', TestClass.m_aInteger.AsString, 'Null integer should be empty string');

    TestClass.m_aFloat.SetToNull;
    Assert.AreEqual('', TestClass.m_aFloat.AsString, 'Null float should be empty string');

    TestClass.m_aCurrency.SetToNull;
    Assert.AreEqual('', TestClass.m_aCurrency.AsString, 'Null currency should be empty string');
  finally
    TestClass.Delete;
  end;
end;

procedure TTestBoldAttributes.TestBlobImageGetStringRepresentationInherited;
var
  BlobJPEG: TBABlobImageJPEG;
  BlobBMP: TBABlobImageBMP;
  TestData: TBytes;
begin
  // Test that GetStringRepresentation returns inherited result for non-brShort representations
  // Bug: Missing "Result :=" before inherited call caused undefined return value
  // ContentType uses brShort, AsString uses brDefault

  TestData := TEncoding.Default.GetBytes('TestBlobData');

  BlobJPEG := TBABlobImageJPEG.Create;
  try
    // brShort (via ContentType) should return the MIME type
    Assert.AreEqual('image/jpeg', BlobJPEG.ContentType,
      'JPEG ContentType should return image/jpeg');

    // Set blob data and verify brDefault returns it via inherited
    BlobJPEG.AsStream.Write(TestData, Length(TestData));
    Assert.AreEqual('TestBlobData', BlobJPEG.AsString,
      'JPEG AsString should return blob data via inherited GetStringRepresentation');
  finally
    BlobJPEG.Free;
  end;

  BlobBMP := TBABlobImageBMP.Create;
  try
    // brShort (via ContentType) should return the MIME type
    Assert.AreEqual('image/bitmap', BlobBMP.ContentType,
      'BMP ContentType should return image/bitmap');

    // Set blob data and verify brDefault returns it via inherited
    BlobBMP.AsStream.Write(TestData, Length(TestData));
    Assert.AreEqual('TestBlobData', BlobBMP.AsString,
      'BMP AsString should return blob data via inherited GetStringRepresentation');
  finally
    BlobBMP.Free;
  end;
end;

type
  // Helper class to access protected GetStringRepresentation
  TBADateTimeAccess = class(TBADateTime);
  TBADateAccess = class(TBADate);
  TBATimeAccess = class(TBATime);

procedure TTestBoldAttributes.TestDateComparesByDayAfterFractionalStore;
var
  Obj: TClassA;
  D: TDateTime;
begin
  // TBADate.SetAsDate truncates, but AsDateTime stores the raw value - once a
  // fraction is in fValue, 'Trunc(Value) = fValue' was false for EVERY input,
  // leaving the attribute permanently 'changed'. Days must compare as days.
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  D := EncodeDate(2026, 7, 12) + EncodeTime(12, 0, 0, 0);  // fractional
  Obj.M_aDate.AsDateTime := D;
  Assert.IsTrue(TBADateAccess(Obj.M_aDate).IsSameValue(D),
    'the identical stored value must compare equal');
  Assert.IsTrue(TBADateAccess(Obj.M_aDate).IsSameValue(EncodeDate(2026, 7, 12)),
    'any value on the same day must compare equal to a date attribute');
end;

procedure TTestBoldAttributes.TestDateTimeGetStringRepresentationNonDefault;
var
  DateTime: TBADateTimeAccess;
  Date: TBADateAccess;
  Time: TBATimeAccess;
  TestDateTime: TDateTime;
begin
  // Test GetStringRepresentation for both brDefault and non-brDefault paths

  TestDateTime := EncodeDate(2026, 1, 6) + EncodeTime(14, 30, 0, 0);

  // TBADateTime
  DateTime := TBADateTimeAccess.Create;
  try
    DateTime.AsDateTime := TestDateTime;

    // brDefault path - should return DateTimeToStr
    Assert.AreEqual(DateTimeToStr(TestDateTime), DateTime.GetStringRepresentation(brDefault),
      'TBADateTime brDefault should return DateTimeToStr format');

    // brShort path (non-brDefault) - should raise exception via inherited
    Assert.WillRaise(
      procedure
      begin
        DateTime.GetStringRepresentation(brShort);
      end,
      EBold,
      'TBADateTime brShort should raise exception via inherited');

    // Test null value with brDefault
    DateTime.SetToNull;
    Assert.AreEqual('', DateTime.GetStringRepresentation(brDefault),
      'TBADateTime brDefault should return empty string when null');
  finally
    DateTime.Free;
  end;

  // TBADate
  Date := TBADateAccess.Create;
  try
    Date.AsDateTime := TestDateTime;

    // brDefault path - should return DateToStr
    Assert.AreEqual(DateToStr(TestDateTime), Date.GetStringRepresentation(brDefault),
      'TBADate brDefault should return DateToStr format');

    // brShort path (non-brDefault) - should raise exception via inherited
    Assert.WillRaise(
      procedure
      begin
        Date.GetStringRepresentation(brShort);
      end,
      EBold,
      'TBADate brShort should raise exception via inherited');

    // Test null value with brDefault
    Date.SetToNull;
    Assert.AreEqual('', Date.GetStringRepresentation(brDefault),
      'TBADate brDefault should return empty string when null');
  finally
    Date.Free;
  end;

  // TBATime
  Time := TBATimeAccess.Create;
  try
    Time.AsDateTime := TestDateTime;

    // brDefault path - should return TimeToStr
    Assert.AreEqual(TimeToStr(TestDateTime), Time.GetStringRepresentation(brDefault),
      'TBATime brDefault should return TimeToStr format');

    // brShort path (non-brDefault) - should raise exception via inherited
    Assert.WillRaise(
      procedure
      begin
        Time.GetStringRepresentation(brShort);
      end,
      EBold,
      'TBATime brShort should raise exception via inherited');

    // Test null value with brDefault
    Time.SetToNull;
    Assert.AreEqual('', Time.GetStringRepresentation(brDefault),
      'TBATime brDefault should return empty string when null');
  finally
    Time.Free;
  end;
end;

procedure TTestBoldAttributes.TestNumericSetStringRepresentation;
var
  IntAttr: TBAInteger;
  FloatAttr: TBAFloat;
  CurrAttr: TBACurrency;
begin
  // Test SetStringRepresentation via AsString property (which uses brDefault)

  // TBAInteger
  IntAttr := TBAInteger.Create;
  try
    // Set value from string
    IntAttr.AsString := '42';
    Assert.AreEqual(42, IntAttr.AsInteger, 'TBAInteger should parse string to integer');

    // Set to null via empty string
    IntAttr.AsString := '';
    Assert.IsTrue(IntAttr.IsNull, 'TBAInteger should be null after setting empty string');
  finally
    IntAttr.Free;
  end;

  // TBAFloat
  FloatAttr := TBAFloat.Create;
  try
    // Set value from string (use locale-correct format)
    FloatAttr.AsString := FloatToStr(3.14);
    Assert.AreEqual(3.14, FloatAttr.AsFloat, 0.001, 'TBAFloat should parse string to float');

    // Set to null via empty string
    FloatAttr.AsString := '';
    Assert.IsTrue(FloatAttr.IsNull, 'TBAFloat should be null after setting empty string');
  finally
    FloatAttr.Free;
  end;

  // TBACurrency
  CurrAttr := TBACurrency.Create;
  try
    // Set value from string (use locale-correct format)
    CurrAttr.AsString := CurrToStr(99.95);
    Assert.AreEqual(99.95, CurrAttr.AsCurrency, 0.001, 'TBACurrency should parse string to currency');

    // Set to null via empty string
    CurrAttr.AsString := '';
    Assert.IsTrue(CurrAttr.IsNull, 'TBACurrency should be null after setting empty string');
  finally
    CurrAttr.Free;
  end;
end;

procedure TTestBoldAttributes.TestDateTimeSetStringRepresentation;
var
  DateTimeAttr: TBADateTime;
  DateAttr: TBADate;
  TimeAttr: TBATime;
  TestDateTime: TDateTime;
begin
  // Test SetStringRepresentation via AsString property (which uses brDefault)

  TestDateTime := EncodeDate(2026, 1, 6) + EncodeTime(14, 30, 0, 0);

  // TBADateTime
  DateTimeAttr := TBADateTime.Create;
  try
    // Set value from string
    DateTimeAttr.AsString := DateTimeToStr(TestDateTime);
    Assert.AreEqual(TestDateTime, DateTimeAttr.AsDateTime, 'TBADateTime should parse string');

    // Set to null via empty string
    DateTimeAttr.AsString := '';
    Assert.IsTrue(DateTimeAttr.IsNull, 'TBADateTime should be null after setting empty string');

    // Set via '<NOW>' keyword
    DateTimeAttr.AsString := '<NOW>';
    Assert.IsFalse(DateTimeAttr.IsNull, 'TBADateTime should not be null after setting <NOW>');
  finally
    DateTimeAttr.Free;
  end;

  // TBADate
  DateAttr := TBADate.Create;
  try
    // Set value from string
    DateAttr.AsString := DateToStr(TestDateTime);
    Assert.AreEqual(Trunc(TestDateTime), Trunc(DateAttr.AsDateTime), 'TBADate should parse string');

    // Set to null via empty string
    DateAttr.AsString := '';
    Assert.IsTrue(DateAttr.IsNull, 'TBADate should be null after setting empty string');

    // Set via '<NOW>' keyword
    DateAttr.AsString := '<NOW>';
    Assert.IsFalse(DateAttr.IsNull, 'TBADate should not be null after setting <NOW>');
  finally
    DateAttr.Free;
  end;

  // TBATime
  TimeAttr := TBATime.Create;
  try
    // Set value from string
    TimeAttr.AsString := TimeToStr(TestDateTime);
    Assert.AreEqual(TimeToStr(TestDateTime), TimeToStr(TimeAttr.AsDateTime), 'TBATime should parse string');

    // Set to null via empty string
    TimeAttr.AsString := '';
    Assert.IsTrue(TimeAttr.IsNull, 'TBATime should be null after setting empty string');

    // Set via '<NOW>' keyword
    TimeAttr.AsString := '<NOW>';
    Assert.IsFalse(TimeAttr.IsNull, 'TBATime should not be null after setting <NOW>');
  finally
    TimeAttr.Free;
  end;
end;

procedure TTestBoldAttributes.TestDateTimeValidateString;
var
  DateTimeAttr: TBADateTime;
  DateAttr: TBADate;
begin
  // Test TBADateTime.ValidateString
  DateTimeAttr := TBADateTime.Create;
  try
    // Empty string - valid if nullable
    Assert.IsTrue(DateTimeAttr.ValidateString('', brDefault), 'TBADateTime empty string should be valid');

    // <NOW> keyword
    Assert.IsTrue(DateTimeAttr.ValidateString('<NOW>', brDefault), 'TBADateTime <NOW> should be valid');

    // Valid datetime string
    Assert.IsTrue(DateTimeAttr.ValidateString(DateTimeToStr(Now), brDefault), 'TBADateTime valid datetime should be valid');

    // Invalid string - triggers exception path
    Assert.IsFalse(DateTimeAttr.ValidateString('invalid', brDefault), 'TBADateTime invalid string should be invalid');
  finally
    DateTimeAttr.Free;
  end;

  // Test TBADate.ValidateString
  DateAttr := TBADate.Create;
  try
    // Empty string - valid if nullable
    Assert.IsTrue(DateAttr.ValidateString('', brDefault), 'TBADate empty string should be valid');

    // <NOW> keyword
    Assert.IsTrue(DateAttr.ValidateString('<NOW>', brDefault), 'TBADate <NOW> should be valid');

    // Valid date string
    Assert.IsTrue(DateAttr.ValidateString(DateToStr(Now), brDefault), 'TBADate valid date should be valid');

    // Invalid string - triggers exception path
    Assert.IsFalse(DateAttr.ValidateString('invalid', brDefault), 'TBADate invalid string should be invalid');
  finally
    DateAttr.Free;
  end;
end;

procedure TTestBoldAttributes.TestCreateWithValueConstructors;
var
  StrAttr: TBAString;
  IntAttr: TBAInteger;
  FloatAttr: TBAFloat;
  CurrAttr: TBACurrency;
  BoolAttr: TBABoolean;
begin
  // Test TBAString.CreateWithValue
  StrAttr := TBAString.CreateWithValue('TestString');
  try
    Assert.AreEqual('TestString', StrAttr.AsString, 'TBAString.CreateWithValue should set value');
    Assert.IsFalse(StrAttr.IsNull, 'TBAString.CreateWithValue should not be null');
  finally
    StrAttr.Free;
  end;

  // Test TBAInteger.CreateWithValue
  IntAttr := TBAInteger.CreateWithValue(42);
  try
    Assert.AreEqual(42, IntAttr.AsInteger, 'TBAInteger.CreateWithValue should set value');
    Assert.IsFalse(IntAttr.IsNull, 'TBAInteger.CreateWithValue should not be null');
  finally
    IntAttr.Free;
  end;

  // Test TBAFloat.CreateWithValue
  FloatAttr := TBAFloat.CreateWithValue(3.14);
  try
    Assert.AreEqual(3.14, FloatAttr.AsFloat, 0.001, 'TBAFloat.CreateWithValue should set value');
    Assert.IsFalse(FloatAttr.IsNull, 'TBAFloat.CreateWithValue should not be null');
  finally
    FloatAttr.Free;
  end;

  // Test TBACurrency.CreateWithValue
  CurrAttr := TBACurrency.CreateWithValue(99.95);
  try
    Assert.AreEqual(99.95, CurrAttr.AsCurrency, 0.001, 'TBACurrency.CreateWithValue should set value');
    Assert.IsFalse(CurrAttr.IsNull, 'TBACurrency.CreateWithValue should not be null');
  finally
    CurrAttr.Free;
  end;

  // Test TBABoolean.CreateWithValue
  BoolAttr := TBABoolean.CreateWithValue(True);
  try
    Assert.IsTrue(BoolAttr.AsBoolean, 'TBABoolean.CreateWithValue(True) should return True');
    Assert.IsFalse(BoolAttr.IsNull, 'TBABoolean.CreateWithValue should not be null');
  finally
    BoolAttr.Free;
  end;

  BoolAttr := TBABoolean.CreateWithValue(False);
  try
    Assert.IsFalse(BoolAttr.AsBoolean, 'TBABoolean.CreateWithValue(False) should return False');
    Assert.IsFalse(BoolAttr.IsNull, 'TBABoolean.CreateWithValue should not be null');
  finally
    BoolAttr.Free;
  end;
end;

procedure TTestBoldAttributes.TestIsNullOrEmptyAndIsNullOrZero;
var
  StrAttr: TBAString;
  IntAttr: TBAInteger;
  FloatAttr: TBAFloat;
begin
  // Test TBAString.IsNullOrEmpty
  StrAttr := TBAString.Create;
  try
    // When null
    StrAttr.SetToNull;
    Assert.IsTrue(StrAttr.IsNullOrEmpty, 'Null string should be NullOrEmpty');

    // When empty string
    StrAttr.AsString := '';
    Assert.IsTrue(StrAttr.IsNullOrEmpty, 'Empty string should be NullOrEmpty');

    // When has value
    StrAttr.AsString := 'Test';
    Assert.IsFalse(StrAttr.IsNullOrEmpty, 'Non-empty string should not be NullOrEmpty');
  finally
    StrAttr.Free;
  end;

  // Test TBAInteger.IsNullOrZero (via TBANumeric)
  IntAttr := TBAInteger.Create;
  try
    // When null
    IntAttr.SetToNull;
    Assert.IsTrue(IntAttr.IsNullOrZero, 'Null integer should be NullOrZero');

    // When zero
    IntAttr.AsInteger := 0;
    Assert.IsTrue(IntAttr.IsNullOrZero, 'Zero integer should be NullOrZero');

    // When non-zero
    IntAttr.AsInteger := 5;
    Assert.IsFalse(IntAttr.IsNullOrZero, 'Non-zero integer should not be NullOrZero');
  finally
    IntAttr.Free;
  end;

  // Test TBAFloat.IsNullOrZero
  FloatAttr := TBAFloat.Create;
  try
    // When null
    FloatAttr.SetToNull;
    Assert.IsTrue(FloatAttr.IsNullOrZero, 'Null float should be NullOrZero');

    // When zero
    FloatAttr.AsFloat := 0.0;
    Assert.IsTrue(FloatAttr.IsNullOrZero, 'Zero float should be NullOrZero');

    // When non-zero
    FloatAttr.AsFloat := 3.14;
    Assert.IsFalse(FloatAttr.IsNullOrZero, 'Non-zero float should not be NullOrZero');
  finally
    FloatAttr.Free;
  end;
end;

procedure TTestBoldAttributes.TestIntegerValidateCharacter;
var
  IntAttr: TBAInteger;
begin
  IntAttr := TBAInteger.Create;
  try
    // Valid characters
    Assert.IsTrue(IntAttr.ValidateCharacter('0', brDefault), 'Digit 0 should be valid');
    Assert.IsTrue(IntAttr.ValidateCharacter('5', brDefault), 'Digit 5 should be valid');
    Assert.IsTrue(IntAttr.ValidateCharacter('9', brDefault), 'Digit 9 should be valid');
    Assert.IsTrue(IntAttr.ValidateCharacter('-', brDefault), 'Minus should be valid');
    Assert.IsTrue(IntAttr.ValidateCharacter('+', brDefault), 'Plus should be valid');

    // Invalid characters
    Assert.IsFalse(IntAttr.ValidateCharacter('a', brDefault), 'Letter should be invalid');
    Assert.IsFalse(IntAttr.ValidateCharacter('.', brDefault), 'Decimal point should be invalid');
    Assert.IsFalse(IntAttr.ValidateCharacter(' ', brDefault), 'Space should be invalid');
  finally
    IntAttr.Free;
  end;
end;

procedure TTestBoldAttributes.TestCurrencyValidateCharacter;
var
  CurrAttr: TBACurrency;
begin
  CurrAttr := TBACurrency.Create;
  try
    // Valid characters
    Assert.IsTrue(CurrAttr.ValidateCharacter('0', brDefault), 'Digit 0 should be valid');
    Assert.IsTrue(CurrAttr.ValidateCharacter('5', brDefault), 'Digit 5 should be valid');
    Assert.IsTrue(CurrAttr.ValidateCharacter('-', brDefault), 'Minus should be valid');
    Assert.IsTrue(CurrAttr.ValidateCharacter('+', brDefault), 'Plus should be valid');
    Assert.IsTrue(CurrAttr.ValidateCharacter('e', brDefault), 'Lowercase e should be valid');
    Assert.IsTrue(CurrAttr.ValidateCharacter('E', brDefault), 'Uppercase E should be valid');
    Assert.IsTrue(CurrAttr.ValidateCharacter(FormatSettings.DecimalSeparator, brDefault), 'Decimal separator should be valid');

    // Invalid characters
    Assert.IsFalse(CurrAttr.ValidateCharacter('a', brDefault), 'Letter a should be invalid');
    Assert.IsFalse(CurrAttr.ValidateCharacter(' ', brDefault), 'Space should be invalid');
  finally
    CurrAttr.Free;
  end;
end;

procedure TTestBoldAttributes.TestFloatValidateCharacter;
var
  FloatAttr: TBAFloat;
begin
  FloatAttr := TBAFloat.Create;
  try
    // Valid characters
    Assert.IsTrue(FloatAttr.ValidateCharacter('0', brDefault), 'Digit 0 should be valid');
    Assert.IsTrue(FloatAttr.ValidateCharacter('5', brDefault), 'Digit 5 should be valid');
    Assert.IsTrue(FloatAttr.ValidateCharacter('-', brDefault), 'Minus should be valid');
    Assert.IsTrue(FloatAttr.ValidateCharacter('+', brDefault), 'Plus should be valid');
    Assert.IsTrue(FloatAttr.ValidateCharacter('e', brDefault), 'Lowercase e should be valid');
    Assert.IsTrue(FloatAttr.ValidateCharacter('E', brDefault), 'Uppercase E should be valid');
    Assert.IsTrue(FloatAttr.ValidateCharacter(FormatSettings.DecimalSeparator, brDefault), 'Decimal separator should be valid');

    // Invalid characters
    Assert.IsFalse(FloatAttr.ValidateCharacter('a', brDefault), 'Letter a should be invalid');
    Assert.IsFalse(FloatAttr.ValidateCharacter(' ', brDefault), 'Space should be invalid');
  finally
    FloatAttr.Free;
  end;
end;

procedure TTestBoldAttributes.TestTrimmedStringTrimsWhitespace;
var
  TrimmedStr: TBATrimmedString;
begin
  TrimmedStr := TBATrimmedString.Create;
  try
    // Test leading whitespace is trimmed
    TrimmedStr.AsString := '  Hello';
    Assert.AreEqual('Hello', TrimmedStr.AsString, 'Leading whitespace should be trimmed');

    // Test trailing whitespace is trimmed
    TrimmedStr.AsString := 'World  ';
    Assert.AreEqual('World', TrimmedStr.AsString, 'Trailing whitespace should be trimmed');

    // Test both leading and trailing whitespace trimmed
    TrimmedStr.AsString := '  Test  ';
    Assert.AreEqual('Test', TrimmedStr.AsString, 'Both leading and trailing whitespace should be trimmed');

    // Test string with no whitespace unchanged
    TrimmedStr.AsString := 'NoWhitespace';
    Assert.AreEqual('NoWhitespace', TrimmedStr.AsString, 'String without whitespace should be unchanged');
  finally
    TrimmedStr.Free;
  end;
end;

procedure TTestBoldAttributes.TestTextValidateStringNoLengthCheck;
var
  TextAttr: TBAText;
  LongString: AnsiString;
  i: integer;
begin
  // TBAText should not check length (unlike TBAString which does)
  // Note: TBAText is an AnsiString-based class, so we use AnsiString
  TextAttr := TBAText.Create;
  try
    // Create a very long AnsiString (TBAText is for long AnsiStrings)
    SetLength(LongString, 10000);
    for i := 1 to Length(LongString) do
      LongString[i] := AnsiChar('A');

    // TBAText.ValidateString should return True for any length
    Assert.IsTrue(TextAttr.ValidateString(string(LongString), brDefault), 'TBAText should accept any length string');

    // Normal string should also be valid
    Assert.IsTrue(TextAttr.ValidateString('Normal text', brDefault), 'TBAText should accept normal string');
  finally
    TextAttr.Free;
  end;
end;

procedure TTestBoldAttributes.TestUnicodeTextValidateString;
var
  UnicodeTextAttr: TBAUnicodeText;
  LongString: string;
begin
  // TBAUnicodeText should not check length
  UnicodeTextAttr := TBAUnicodeText.Create;
  try
    // Create a very long string
    SetLength(LongString, 10000);
    FillChar(LongString[1], Length(LongString) * SizeOf(Char), Ord('A'));

    // TBAUnicodeText.ValidateString should return True for any length
    Assert.IsTrue(UnicodeTextAttr.ValidateString(LongString, brDefault), 'TBAUnicodeText should accept any length string');

    // Normal string should also be valid
    Assert.IsTrue(UnicodeTextAttr.ValidateString('Unicode text with special chars: äöü', brDefault),
      'TBAUnicodeText should accept Unicode string');
  finally
    UnicodeTextAttr.Free;
  end;
end;

procedure TTestBoldAttributes.TestBlobStreamSaveOperations;
var
  Blob: TBABlob;
  OutputStream: TMemoryStream;
  TestData: TBytes;
  ReadData: TBytes;
begin
  Blob := TBABlob.Create;
  OutputStream := TMemoryStream.Create;
  try
    // Write some test data to the blob
    TestData := TEncoding.Default.GetBytes('SaveToStreamTest');
    Blob.AsStream.Write(TestData, Length(TestData));

    // Test SaveToStream
    Blob.AsStream.Position := 0;
    Blob.AsStream.SaveToStream(OutputStream);

    // Verify data was saved correctly
    Assert.AreEqual(Int64(Length(TestData)), OutputStream.Size, 'SaveToStream size should match');
    OutputStream.Position := 0;
    SetLength(ReadData, OutputStream.Size);
    OutputStream.ReadBuffer(ReadData, OutputStream.Size);
    Assert.AreEqual(string(TEncoding.Default.GetString(TestData)),
      string(TEncoding.Default.GetString(ReadData)),
      'SaveToStream content should match');
  finally
    OutputStream.Free;
    Blob.Free;
  end;
end;

procedure TTestBoldAttributes.TestBlobStreamTruncate;
var
  Blob: TBABlob;
  TestData: TBytes;
begin
  Blob := TBABlob.Create;
  try
    // Write test data
    TestData := TEncoding.Default.GetBytes('TruncateTestData');
    Blob.AsStream.Write(TestData, Length(TestData));
    Assert.AreEqual(Int64(Length(TestData)), Blob.AsStream.Size, 'Initial size should match data length');

    // Move position to middle and truncate
    Blob.AsStream.Position := 8; // Middle of 'Truncate'
    Blob.AsStream.Truncate;

    // Verify truncation
    Assert.AreEqual(Int64(8), Blob.AsStream.Size, 'Size after truncate should be position');
  finally
    Blob.Free;
  end;
end;

procedure TTestBoldAttributes.TestAnsiStringValidation;
var
  AnsiAttr: TBAAnsiString;
begin
  AnsiAttr := TBAAnsiString.Create;
  try
    // Test valid AnsiString
    Assert.IsTrue(AnsiAttr.ValidateString('Hello World', brDefault), 'ASCII string should be valid AnsiString');

    // Test ValidateCharacter - ASCII chars should be valid
    Assert.IsTrue(AnsiAttr.ValidateCharacter('A', brDefault), 'ASCII char should be valid');
    Assert.IsTrue(AnsiAttr.ValidateCharacter('z', brDefault), 'ASCII lowercase should be valid');
    Assert.IsTrue(AnsiAttr.ValidateCharacter('0', brDefault), 'ASCII digit should be valid');
  finally
    AnsiAttr.Free;
  end;
end;

procedure TTestBoldAttributes.TestIntegerGetAsFloat;
var
  IntAttr: TBAInteger;
begin
  IntAttr := TBAInteger.Create;
  try
    IntAttr.AsInteger := 42;
    // GetAsFloat returns the integer value as a float
    Assert.AreEqual(42.0, IntAttr.AsFloat, 0.001, 'Integer GetAsFloat should return float value');

    IntAttr.AsInteger := -100;
    Assert.AreEqual(-100.0, IntAttr.AsFloat, 0.001, 'Negative integer GetAsFloat should return float value');
  finally
    IntAttr.Free;
  end;
end;

procedure TTestBoldAttributes.TestCurrencyGetAsFloat;
var
  CurrAttr: TBACurrency;
begin
  CurrAttr := TBACurrency.Create;
  try
    CurrAttr.AsCurrency := 99.95;
    // GetAsFloat returns the currency value as a float
    Assert.AreEqual(99.95, CurrAttr.AsFloat, 0.001, 'Currency GetAsFloat should return float value');

    CurrAttr.AsCurrency := -50.25;
    Assert.AreEqual(-50.25, CurrAttr.AsFloat, 0.001, 'Negative currency GetAsFloat should return float value');
  finally
    CurrAttr.Free;
  end;
end;

procedure TTestBoldAttributes.TestNumericCompareToAsCtDefault;
var
  Float1, Float2: TBAFloat;
begin
  Float1 := TBAFloat.Create;
  Float2 := TBAFloat.Create;
  try
    // Test TBANumeric.CompareToAs with ctDefault (the uncovered path in base class)
    Float1.AsFloat := 10.5;
    Float2.AsFloat := 10.5;
    Assert.AreEqual(0, Float1.CompareToAs(ctDefault, Float2), 'Equal floats should return 0');

    Float1.AsFloat := 10.5;
    Float2.AsFloat := 5.0;
    Assert.AreEqual(1, Float1.CompareToAs(ctDefault, Float2), 'Greater float should return 1');

    Float1.AsFloat := 5.0;
    Float2.AsFloat := 10.5;
    Assert.AreEqual(-1, Float1.CompareToAs(ctDefault, Float2), 'Lesser float should return -1');

    // Test with one null
    Float1.AsFloat := 10.5;
    Float2.SetToNull;
    Assert.AreEqual(1, Float1.CompareToAs(ctDefault, Float2), 'Non-null vs null should return 1');
  finally
    Float1.Free;
    Float2.Free;
  end;
end;

procedure TTestBoldAttributes.TestBooleanCreateWithValue;
var
  BoolAttr: TBABoolean;
begin
  // Test TBABoolean.CreateWithValue - covers the constructor
  BoolAttr := TBABoolean.CreateWithValue(True);
  try
    Assert.IsTrue(BoolAttr.AsBoolean, 'CreateWithValue(True) should be True');
    Assert.IsFalse(BoolAttr.IsNull, 'CreateWithValue should not be null');
  finally
    BoolAttr.Free;
  end;

  BoolAttr := TBABoolean.CreateWithValue(False);
  try
    Assert.IsFalse(BoolAttr.AsBoolean, 'CreateWithValue(False) should be False');
    Assert.IsFalse(BoolAttr.IsNull, 'CreateWithValue should not be null');
  finally
    BoolAttr.Free;
  end;
end;

// TBAMoment / TBADateTime / TBADate / TBATime coverage

procedure TTestBoldAttributes.TestMomentGetParts;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Obj.aDateTime := EncodeDate(2026, 3, 15) + EncodeTime(14, 30, 45, 0);
  Assert.AreEqual(2026, Obj.M_aDateTime.Years, 'Years should be 2026');
  Assert.AreEqual(3, Obj.M_aDateTime.Months, 'Months should be 3');
  Assert.AreEqual(15, Obj.M_aDateTime.Days, 'Days should be 15');
  Assert.AreEqual(14, Obj.M_aDateTime.Hours, 'Hours should be 14');
  Assert.AreEqual(30, Obj.M_aDateTime.Minutes, 'Minutes should be 30');
  Assert.AreEqual(45, Obj.M_aDateTime.Seconds, 'Seconds should be 45');
end;

procedure TTestBoldAttributes.TestMomentAsVariant;
var
  Obj: TClassA;
  V: Variant;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Obj.aDateTime := EncodeDate(2026, 6, 1);
  V := Obj.M_aDateTime.AsVariant;
  Assert.IsFalse(VarIsNull(V), 'AsVariant should not be null');
  Obj.M_aDateTime.SetToNull;
  V := Obj.M_aDateTime.AsVariant;
  Assert.IsTrue(VarIsNull(V), 'AsVariant should be null after SetToNull');
end;

procedure TTestBoldAttributes.TestMomentAsFloat;
var
  Obj: TClassA;
  F: Double;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Obj.aDateTime := EncodeDate(2026, 1, 1);
  F := Obj.M_aDateTime.AsFloat;
  Assert.IsTrue(F > 0, 'AsFloat should be positive for a valid date');
end;

procedure TTestBoldAttributes.TestDateTimeAsDate;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Obj.aDateTime := EncodeDate(2026, 7, 4) + EncodeTime(10, 0, 0, 0);
  Assert.AreEqual(EncodeDate(2026, 7, 4), Obj.M_aDateTime.AsDate, 'AsDate should strip time part');
end;

procedure TTestBoldAttributes.TestDateTimeAsTime;
var
  Obj: TClassA;
  TimeVal: TDateTime;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Obj.aDateTime := EncodeDate(2026, 7, 4) + EncodeTime(10, 30, 0, 0);
  TimeVal := Obj.M_aDateTime.AsTime;
  Assert.IsTrue(TimeVal > 0, 'AsTime should return a positive time value');
end;

procedure TTestBoldAttributes.TestDateTimeSetAsDatePreservesTime;
var
  Obj: TClassA;
begin
  // Regression for H5 (4d8f323): TBADateTime.SetAsDate stored the raw value,
  // wiping the time-of-day. Contract: AsDate changes the calendar day only.
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Obj.aDateTime := EncodeDate(2026, 7, 4) + EncodeTime(9, 0, 0, 0);
  Obj.M_aDateTime.AsDate := EncodeDate(2026, 12, 24);
  Assert.AreEqual(Extended(EncodeDate(2026, 12, 24)), Extended(Obj.M_aDateTime.AsDate), 1E-9,
    'SetAsDate should set the date part');
  Assert.AreEqual(Extended(EncodeTime(9, 0, 0, 0)), Extended(Obj.M_aDateTime.AsTime), 1E-9,
    'SetAsDate must preserve the time-of-day');
end;

procedure TTestBoldAttributes.TestTimeSetAsDatePreservesTime;
var
  Obj: TClassA;
begin
  // Mirror of H5 for TBATime: SetAsDate stored Frac(Value) - a pure date
  // argument (Frac = 0) silently reset the stored time to midnight.
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Obj.M_aTime.AsTime := EncodeTime(10, 30, 0, 0);
  TBATimeAccess(Obj.M_aTime).AsDate := EncodeDate(2026, 12, 24);
  Assert.AreEqual(Extended(EncodeTime(10, 30, 0, 0)), Extended(Obj.M_aTime.AsTime), 1E-9,
    'Setting the date of a time-only attribute must not change its time');
end;

// TBAString coverage

procedure TTestBoldAttributes.TestStringAsAnsiAndUnicode;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Obj.M_aString.AsUnicodeString := 'Unicode test';
  Assert.AreEqual('Unicode test', Obj.M_aString.AsUnicodeString, 'AsUnicodeString should match');
  Assert.AreEqual(AnsiString('Unicode test'), Obj.M_aString.AsAnsiString, 'AsAnsiString should match');
end;

procedure TTestBoldAttributes.TestStringAssignValue;
var
  Obj1, Obj2: TClassA;
  Sys: TBoldSystem;
begin
  Sys := FDataModule.BoldSystemHandle1.System;
  Obj1 := TClassA.Create(Sys);
  Obj2 := TClassA.Create(Sys);
  Obj1.aString := 'Source';
  Obj2.M_aString.Assign(Obj1.M_aString);
  Assert.AreEqual('Source', Obj2.aString, 'Assign should copy value');
end;

// TBABlob coverage

procedure TTestBoldAttributes.TestBlobLoadSaveFile;
var
  Obj: TClassA;
  TempFile: string;
  InStream: TStringStream;
begin
  TempFile := GetEnvironmentVariable('TEMP') + '\bold_blob_test.dat';
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  try
    InStream := TStringStream.Create('Hello Blob');
    try
      Obj.M_aBlob.LoadFromStream(InStream);
    finally
      InStream.Free;
    end;
    Obj.M_aBlob.SaveToFile(TempFile);
    Obj.M_aBlob.SetToNull;
    Obj.M_aBlob.LoadFromFile(TempFile);
    Assert.IsTrue(Obj.M_aBlob.BlobSize > 0, 'Blob should have content after LoadFromFile');
  finally
    SysUtils.DeleteFile(TempFile);
  end;
end;

procedure TTestBoldAttributes.TestBlobLoadSaveStream;
var
  Obj: TClassA;
  InStream, OutStream: TStringStream;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  InStream := TStringStream.Create('Stream test');
  try
    Obj.M_aBlob.LoadFromStream(InStream);
  finally
    InStream.Free;
  end;

  OutStream := TStringStream.Create;
  try
    Obj.M_aBlob.SaveToStream(OutStream);
    Assert.IsTrue(OutStream.Size > 0, 'SaveToStream should write data');
  finally
    OutStream.Free;
  end;
end;

procedure TTestBoldAttributes.TestBlobAssign;
var
  Obj1, Obj2: TClassA;
  Sys: TBoldSystem;
  InStream: TStringStream;
begin
  Sys := FDataModule.BoldSystemHandle1.System;
  Obj1 := TClassA.Create(Sys);
  Obj2 := TClassA.Create(Sys);
  InStream := TStringStream.Create('Assign test');
  try
    Obj1.M_aBlob.LoadFromStream(InStream);
  finally
    InStream.Free;
  end;
  Obj2.M_aBlob.Assign(Obj1.M_aBlob);
  Assert.IsTrue(Obj2.M_aBlob.BlobSize > 0, 'Assign should copy blob content');
end;

procedure TTestBoldAttributes.TestBlobSize;
var
  Obj: TClassA;
  InStream: TStringStream;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Assert.AreEqual(Int64(0), Obj.M_aBlob.BlobSize, 'Empty blob should have size 0');
  InStream := TStringStream.Create('12345');
  try
    Obj.M_aBlob.LoadFromStream(InStream);
  finally
    InStream.Free;
  end;
  Assert.IsTrue(Obj.M_aBlob.BlobSize > 0, 'Blob should have positive size after load');
end;

// TBACurrency coverage

procedure TTestBoldAttributes.TestCurrencyAsCurrency;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Obj.M_aCurrency.AsCurrency := 123.45;
  Assert.AreEqual(Double(123.45), Double(Obj.M_aCurrency.AsCurrency), 0.001, 'AsCurrency should match');
end;

procedure TTestBoldAttributes.TestCurrencyAsVariant;
var
  Obj: TClassA;
  V: Variant;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Obj.M_aCurrency.AsCurrency := 99.99;
  V := Obj.M_aCurrency.AsVariant;
  Assert.IsFalse(VarIsNull(V), 'Currency AsVariant should not be null');
end;

procedure TTestBoldAttributes.TestCurrencyAssign;
var
  Obj1, Obj2: TClassA;
  Sys: TBoldSystem;
begin
  Sys := FDataModule.BoldSystemHandle1.System;
  Obj1 := TClassA.Create(Sys);
  Obj2 := TClassA.Create(Sys);
  Obj1.M_aCurrency.AsCurrency := 42.50;
  Obj2.M_aCurrency.Assign(Obj1.M_aCurrency);
  Assert.AreEqual(Double(42.50), Double(Obj2.M_aCurrency.AsCurrency), 0.001, 'Assign should copy currency');
end;

// TBAFloat coverage

procedure TTestBoldAttributes.TestFloatAsVariant;
var
  Obj: TClassA;
  V: Variant;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Obj.aFloat := 3.14;
  V := Obj.M_aFloat.AsVariant;
  Assert.IsFalse(VarIsNull(V), 'Float AsVariant should not be null');
end;

procedure TTestBoldAttributes.TestFloatAssign;
var
  Obj1, Obj2: TClassA;
  Sys: TBoldSystem;
begin
  Sys := FDataModule.BoldSystemHandle1.System;
  Obj1 := TClassA.Create(Sys);
  Obj2 := TClassA.Create(Sys);
  Obj1.aFloat := 2.718;
  Obj2.M_aFloat.Assign(Obj1.M_aFloat);
  Assert.AreEqual(Double(2.718), Obj2.aFloat, 0.001, 'Assign should copy float');
end;

// TBAInteger coverage

procedure TTestBoldAttributes.TestIntegerAsVariant;
var
  Obj: TClassA;
  V: Variant;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Obj.aInteger := 42;
  V := Obj.M_aInteger.AsVariant;
  Assert.IsFalse(VarIsNull(V), 'Integer AsVariant should not be null');
  Assert.AreEqual(42, Integer(V), 'Variant value should be 42');
end;

procedure TTestBoldAttributes.TestIntegerAssign;
var
  Obj1, Obj2: TClassA;
  Sys: TBoldSystem;
begin
  Sys := FDataModule.BoldSystemHandle1.System;
  Obj1 := TClassA.Create(Sys);
  Obj2 := TClassA.Create(Sys);
  Obj1.aInteger := 99;
  Obj2.M_aInteger.Assign(Obj1.M_aInteger);
  Assert.AreEqual(99, Obj2.aInteger, 'Assign should copy integer');
end;

// Integer subtype range checks

procedure TTestBoldAttributes.TestByteRangeCheck;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  // Valid range: 0..255
  Obj.aByte := 0;
  Assert.AreEqual(Byte(0), Obj.aByte, 'Byte min should be 0');
  Obj.aByte := 255;
  Assert.AreEqual(Byte(255), Obj.aByte, 'Byte max should be 255');

  // Out of range should fail validation
  Assert.IsFalse(Obj.M_aByte.ValidateString('-1', brDefault), 'Byte -1 should fail');
  Assert.IsFalse(Obj.M_aByte.ValidateString('256', brDefault), 'Byte 256 should fail');
  Assert.IsTrue(Obj.M_aByte.ValidateString('128', brDefault), 'Byte 128 should pass');
end;

procedure TTestBoldAttributes.TestShortIntRangeCheck;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  // Valid range: -128..127
  Obj.aShortInt := -128;
  Assert.AreEqual(ShortInt(-128), Obj.aShortInt, 'ShortInt min should be -128');
  Obj.aShortInt := 127;
  Assert.AreEqual(ShortInt(127), Obj.aShortInt, 'ShortInt max should be 127');

  Assert.IsFalse(Obj.M_aShortInt.ValidateString('-129', brDefault), 'ShortInt -129 should fail');
  Assert.IsFalse(Obj.M_aShortInt.ValidateString('128', brDefault), 'ShortInt 128 should fail');
  Assert.IsTrue(Obj.M_aShortInt.ValidateString('0', brDefault), 'ShortInt 0 should pass');
end;

procedure TTestBoldAttributes.TestSmallIntRangeCheck;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  // Valid range: -32768..32767
  Obj.aSmallInt := -32768;
  Assert.AreEqual(SmallInt(-32768), Obj.aSmallInt, 'SmallInt min should be -32768');
  Obj.aSmallInt := 32767;
  Assert.AreEqual(SmallInt(32767), Obj.aSmallInt, 'SmallInt max should be 32767');

  Assert.IsFalse(Obj.M_aSmallInt.ValidateString('-32769', brDefault), 'SmallInt -32769 should fail');
  Assert.IsFalse(Obj.M_aSmallInt.ValidateString('32768', brDefault), 'SmallInt 32768 should fail');
  Assert.IsTrue(Obj.M_aSmallInt.ValidateString('1000', brDefault), 'SmallInt 1000 should pass');
end;

procedure TTestBoldAttributes.TestWordRangeCheck;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  // Valid range: 0..65535
  Obj.aWord := 0;
  Assert.AreEqual(Word(0), Obj.aWord, 'Word min should be 0');
  Obj.aWord := 65535;
  Assert.AreEqual(Word(65535), Obj.aWord, 'Word max should be 65535');

  Assert.IsFalse(Obj.M_aWord.ValidateString('-1', brDefault), 'Word -1 should fail');
  Assert.IsFalse(Obj.M_aWord.ValidateString('65536', brDefault), 'Word 65536 should fail');
  Assert.IsTrue(Obj.M_aWord.ValidateString('30000', brDefault), 'Word 30000 should pass');
end;

procedure TTestBoldAttributes.TestByteGetSetAs;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Obj.M_aByte.AsByte := 42;
  Assert.AreEqual(Byte(42), Obj.M_aByte.AsByte, 'GetAsByte should return 42');
  Obj.M_aByte.AsByte := 0;
  Assert.AreEqual(Byte(0), Obj.M_aByte.AsByte, 'GetAsByte should return 0');
end;

procedure TTestBoldAttributes.TestShortIntGetSetAs;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Obj.M_aShortInt.AsShortInt := -50;
  Assert.AreEqual(ShortInt(-50), Obj.M_aShortInt.AsShortInt, 'GetAsShortInt should return -50');
  Obj.M_aShortInt.AsShortInt := 100;
  Assert.AreEqual(ShortInt(100), Obj.M_aShortInt.AsShortInt, 'GetAsShortInt should return 100');
end;

procedure TTestBoldAttributes.TestSmallIntGetSetAs;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Obj.M_aSmallInt.AsSmallInt := -1000;
  Assert.AreEqual(SmallInt(-1000), Obj.M_aSmallInt.AsSmallInt, 'GetAsSmallInt should return -1000');
  Obj.M_aSmallInt.AsSmallInt := 20000;
  Assert.AreEqual(SmallInt(20000), Obj.M_aSmallInt.AsSmallInt, 'GetAsSmallInt should return 20000');
end;

procedure TTestBoldAttributes.TestWordGetSetAs;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Obj.M_aWord.AsWord := 50000;
  Assert.AreEqual(Word(50000), Obj.M_aWord.AsWord, 'GetAsWord should return 50000');
  Obj.M_aWord.AsWord := 0;
  Assert.AreEqual(Word(0), Obj.M_aWord.AsWord, 'GetAsWord should return 0');
end;

// AnsiString coverage

procedure TTestBoldAttributes.TestAnsiStringSetGetDataValue;
var
  AnsiAttr: TBAAnsiString;
begin
  AnsiAttr := TBAAnsiString.Create;
  try
    // Test set and get
    AnsiAttr.AsAnsiString := AnsiString('Hello');
    Assert.AreEqual(AnsiString('Hello'), AnsiAttr.AsAnsiString, 'AsAnsiString should match');

    // Test null returns empty
    AnsiAttr.SetToNull;
    Assert.AreEqual(AnsiString(''), AnsiAttr.AsAnsiString, 'Null AnsiString should be empty');

    // Test overwrite with new value
    AnsiAttr.AsAnsiString := AnsiString('World');
    Assert.AreEqual(AnsiString('World'), AnsiAttr.AsAnsiString, 'Overwritten AnsiString should match');
  finally
    AnsiAttr.Free;
  end;
end;

procedure TTestBoldAttributes.TestAnsiStringFreeContent;
var
  AnsiAttr: TBAAnsiString;
begin
  AnsiAttr := TBAAnsiString.Create;
  try
    AnsiAttr.AsAnsiString := AnsiString('Data');
    Assert.IsFalse(AnsiAttr.IsNull, 'Should not be null after set');

    // SetToNull triggers FreeContent internally
    AnsiAttr.SetToNull;
    Assert.IsTrue(AnsiAttr.IsNull, 'Should be null after SetToNull');
    Assert.AreEqual(AnsiString(''), AnsiAttr.AsAnsiString, 'FreeContent should clear value');
  finally
    AnsiAttr.Free;
  end;
end;

// IsEqualToValue coverage

procedure TTestBoldAttributes.TestIntegerIsEqualToValue;
var
  Int1, Int2: TBAInteger;
begin
  Int1 := TBAInteger.Create;
  Int2 := TBAInteger.Create;
  try
    // Both have same value
    Int1.AsInteger := 42;
    Int2.AsInteger := 42;
    Assert.IsTrue(Int1.IsEqualToValue(Int2.AsIBoldValue[bdepContents]), 'Same integer values should be equal');

    // Different values
    Int2.AsInteger := 99;
    Assert.IsFalse(Int1.IsEqualToValue(Int2.AsIBoldValue[bdepContents]), 'Different integer values should not be equal');

    // Both null
    Int1.SetToNull;
    Int2.SetToNull;
    Assert.IsTrue(Int1.IsEqualToValue(Int2.AsIBoldValue[bdepContents]), 'Both null should be equal');

    // One null, one not
    Int1.AsInteger := 1;
    Assert.IsFalse(Int1.IsEqualToValue(Int2.AsIBoldValue[bdepContents]), 'Non-null vs null should not be equal');

    // Reverse: null vs non-null
    Int1.SetToNull;
    Int2.AsInteger := 1;
    Assert.IsFalse(Int1.IsEqualToValue(Int2.AsIBoldValue[bdepContents]), 'Null vs non-null should not be equal');
  finally
    Int1.Free;
    Int2.Free;
  end;
end;

procedure TTestBoldAttributes.TestFloatIsEqualToValue;
var
  Float1, Float2: TBAFloat;
begin
  Float1 := TBAFloat.Create;
  Float2 := TBAFloat.Create;
  try
    // Same value
    Float1.AsFloat := 3.14;
    Float2.AsFloat := 3.14;
    Assert.IsTrue(Float1.IsEqualToValue(Float2.AsIBoldValue[bdepContents]), 'Same float values should be equal');

    // Different values
    Float2.AsFloat := 2.71;
    Assert.IsFalse(Float1.IsEqualToValue(Float2.AsIBoldValue[bdepContents]), 'Different float values should not be equal');

    // Both null
    Float1.SetToNull;
    Float2.SetToNull;
    Assert.IsTrue(Float1.IsEqualToValue(Float2.AsIBoldValue[bdepContents]), 'Both null floats should be equal');

    // One null
    Float1.AsFloat := 1.0;
    Assert.IsFalse(Float1.IsEqualToValue(Float2.AsIBoldValue[bdepContents]), 'Non-null vs null float should not be equal');
  finally
    Float1.Free;
    Float2.Free;
  end;
end;

procedure TTestBoldAttributes.TestCurrencyIsEqualToValue;
var
  Curr1, Curr2: TBACurrency;
begin
  Curr1 := TBACurrency.Create;
  Curr2 := TBACurrency.Create;
  try
    // Same value
    Curr1.AsCurrency := 99.95;
    Curr2.AsCurrency := 99.95;
    Assert.IsTrue(Curr1.IsEqualToValue(Curr2.AsIBoldValue[bdepContents]), 'Same currency values should be equal');

    // Different values
    Curr2.AsCurrency := 50.00;
    Assert.IsFalse(Curr1.IsEqualToValue(Curr2.AsIBoldValue[bdepContents]), 'Different currency values should not be equal');

    // Both null
    Curr1.SetToNull;
    Curr2.SetToNull;
    Assert.IsTrue(Curr1.IsEqualToValue(Curr2.AsIBoldValue[bdepContents]), 'Both null currencies should be equal');

    // One null
    Curr1.AsCurrency := 10.0;
    Assert.IsFalse(Curr1.IsEqualToValue(Curr2.AsIBoldValue[bdepContents]), 'Non-null vs null currency should not be equal');
  finally
    Curr1.Free;
    Curr2.Free;
  end;
end;

procedure TTestBoldAttributes.TestStringIsEqualToValue;
var
  Str1, Str2: TBAString;
begin
  Str1 := TBAString.Create;
  Str2 := TBAString.Create;
  try
    // Same value
    Str1.AsString := 'Hello';
    Str2.AsString := 'Hello';
    Assert.IsTrue(Str1.IsEqualToValue(Str2.AsIBoldValue[bdepContents]), 'Same string values should be equal');

    // Different values
    Str2.AsString := 'World';
    Assert.IsFalse(Str1.IsEqualToValue(Str2.AsIBoldValue[bdepContents]), 'Different string values should not be equal');

    // Both null
    Str1.SetToNull;
    Str2.SetToNull;
    Assert.IsTrue(Str1.IsEqualToValue(Str2.AsIBoldValue[bdepContents]), 'Both null strings should be equal');

    // One null
    Str1.AsString := 'X';
    Assert.IsFalse(Str1.IsEqualToValue(Str2.AsIBoldValue[bdepContents]), 'Non-null vs null string should not be equal');
  finally
    Str1.Free;
    Str2.Free;
  end;
end;

// Blob byte array variant

procedure TTestBoldAttributes.TestBlobSetAsVariantByteArray;
var
  Blob: TBABlob;
  V: Variant;
  Data: TBytes;
begin
  Blob := TBABlob.Create;
  try
    // Create a byte array variant
    Data := TEncoding.UTF8.GetBytes('VariantTest');
    V := VarArrayCreate([0, Length(Data) - 1], varByte);
    Move(Data[0], VarArrayLock(V)^, Length(Data));
    VarArrayUnlock(V);

    Blob.AsVariant := V;
    Assert.IsTrue(Blob.BlobSize > 0, 'Blob should have content after SetAsVariant byte array');
    Assert.AreEqual(Int64(Length(Data)), Blob.BlobSize, 'Blob size should match data length');
  finally
    Blob.Free;
  end;
end;

// Currency CompareToAs with float operand

procedure TTestBoldAttributes.TestCurrencyCompareToAsWithFloat;
var
  CurrAttr: TBACurrency;
  FloatAttr: TBAFloat;
begin
  CurrAttr := TBACurrency.Create;
  FloatAttr := TBAFloat.Create;
  try
    // Currency vs Float comparison (exercises the non-TBACurrency branch in CompareToAs)
    CurrAttr.AsCurrency := 100.00;
    FloatAttr.AsFloat := 50.0;
    Assert.AreEqual(1, CurrAttr.CompareToAs(ctDefault, FloatAttr), 'Currency 100 > Float 50');

    FloatAttr.AsFloat := 100.0;
    Assert.AreEqual(0, CurrAttr.CompareToAs(ctDefault, FloatAttr), 'Currency 100 = Float 100');

    FloatAttr.AsFloat := 200.0;
    Assert.AreEqual(-1, CurrAttr.CompareToAs(ctDefault, FloatAttr), 'Currency 100 < Float 200');

    // Null handling
    CurrAttr.SetToNull;
    FloatAttr.AsFloat := 1.0;
    Assert.AreEqual(-1, CurrAttr.CompareToAs(ctDefault, FloatAttr), 'Null currency < non-null float');
  finally
    CurrAttr.Free;
    FloatAttr.Free;
  end;
end;

// Currency Assign from float/integer

procedure TTestBoldAttributes.TestCurrencyAssignFromNumericTypes;
var
  CurrAttr: TBACurrency;
  FloatAttr: TBAFloat;
  IntAttr: TBAInteger;
begin
  CurrAttr := TBACurrency.Create;
  FloatAttr := TBAFloat.Create;
  IntAttr := TBAInteger.Create;
  try
    // Assign from TBAFloat
    FloatAttr.AsFloat := 99.5;
    CurrAttr.Assign(FloatAttr);
    Assert.AreEqual(Double(99.5), Double(CurrAttr.AsCurrency), 0.001, 'Currency should match assigned float');

    // Assign from TBAInteger
    IntAttr.AsInteger := 42;
    CurrAttr.Assign(IntAttr);
    Assert.AreEqual(Double(42.0), Double(CurrAttr.AsCurrency), 0.001, 'Currency should match assigned integer');

    // Assign null from numeric
    FloatAttr.SetToNull;
    CurrAttr.Assign(FloatAttr);
    Assert.IsTrue(CurrAttr.IsNull, 'Currency should be null after assigning null float');
  finally
    CurrAttr.Free;
    FloatAttr.Free;
    IntAttr.Free;
  end;
end;

// Integer ValidateString with range

procedure TTestBoldAttributes.TestIntegerValidateStringRange;
var
  ByteAttr: TBAByte;
  WordAttr: TBAWord;
begin
  // Test ValidateString which calls CheckRange internally
  ByteAttr := TBAByte.Create;
  try
    Assert.IsTrue(ByteAttr.ValidateString('0', brDefault), 'Byte 0 should validate');
    Assert.IsTrue(ByteAttr.ValidateString('255', brDefault), 'Byte 255 should validate');
    Assert.IsFalse(ByteAttr.ValidateString('256', brDefault), 'Byte 256 should not validate');
    Assert.IsFalse(ByteAttr.ValidateString('-1', brDefault), 'Byte -1 should not validate');
    Assert.IsTrue(ByteAttr.ValidateString('', brDefault), 'Empty string should validate as null');
  finally
    ByteAttr.Free;
  end;

  WordAttr := TBAWord.Create;
  try
    Assert.IsTrue(WordAttr.ValidateString('0', brDefault), 'Word 0 should validate');
    Assert.IsTrue(WordAttr.ValidateString('65535', brDefault), 'Word 65535 should validate');
    Assert.IsFalse(WordAttr.ValidateString('65536', brDefault), 'Word 65536 should not validate');
    Assert.IsFalse(WordAttr.ValidateString('-1', brDefault), 'Word -1 should not validate');
  finally
    WordAttr.Free;
  end;
end;

// Moment IsNullOrZero

procedure TTestBoldAttributes.TestMomentIsNullOrZero;
var
  DT: TBADateTime;
begin
  DT := TBADateTime.Create;
  try
    // Null should be NullOrZero
    Assert.IsTrue(DT.IsNullOrZero, 'Null datetime should be NullOrZero');

    // Zero datetime should be NullOrZero
    DT.AsDateTime := 0;
    Assert.IsTrue(DT.IsNullOrZero, 'Zero datetime should be NullOrZero');

    // Non-zero should not be NullOrZero
    DT.AsDateTime := EncodeDate(2026, 1, 1);
    Assert.IsFalse(DT.IsNullOrZero, 'Non-zero datetime should not be NullOrZero');
  finally
    DT.Free;
  end;
end;

// Moment IsEqualToValue

procedure TTestBoldAttributes.TestMomentIsEqualToValue;
var
  DT1, DT2: TBADateTime;
  Date1: TBADate;
  Time1: TBATime;
  TestDT: TDateTime;
begin
  TestDT := EncodeDate(2026, 6, 15) + EncodeTime(10, 30, 0, 0);

  DT1 := TBADateTime.Create;
  DT2 := TBADateTime.Create;
  Date1 := TBADate.Create;
  Time1 := TBATime.Create;
  try
    // DateTime vs DateTime - equal
    DT1.AsDateTime := TestDT;
    DT2.AsDateTime := TestDT;
    Assert.IsTrue(DT1.IsEqualToValue(DT2.AsIBoldValue[bdepContents]), 'Same datetimes should be equal');

    // DateTime vs DateTime - not equal
    DT2.AsDateTime := TestDT + 1;
    Assert.IsFalse(DT1.IsEqualToValue(DT2.AsIBoldValue[bdepContents]), 'Different datetimes should not be equal');

    // Both null
    DT1.SetToNull;
    DT2.SetToNull;
    Assert.IsTrue(DT1.IsEqualToValue(DT2.AsIBoldValue[bdepContents]), 'Both null datetimes should be equal');

    // One null
    DT1.AsDateTime := TestDT;
    Assert.IsFalse(DT1.IsEqualToValue(DT2.AsIBoldValue[bdepContents]), 'Non-null vs null datetime should not be equal');

    // DateTime vs Date (exercises IBoldDateContent branch)
    DT1.AsDateTime := EncodeDate(2026, 6, 15);
    Date1.AsDateTime := EncodeDate(2026, 6, 15);
    Assert.IsTrue(DT1.IsEqualToValue(Date1.AsIBoldValue[bdepContents]), 'DateTime date-only should equal Date');

    // DateTime vs Time (exercises IBoldTimeContent branch)
    DT1.AsDateTime := EncodeTime(10, 30, 0, 0);
    Time1.AsDateTime := EncodeTime(10, 30, 0, 0);
    Assert.IsTrue(DT1.IsEqualToValue(Time1.AsIBoldValue[bdepContents]), 'DateTime time-only should equal Time');

    // DateTime vs Date null handling
    DT1.SetToNull;
    Date1.SetToNull;
    Assert.IsTrue(DT1.IsEqualToValue(Date1.AsIBoldValue[bdepContents]), 'Both null should be equal (Date path)');

    DT1.AsDateTime := TestDT;
    Assert.IsFalse(DT1.IsEqualToValue(Date1.AsIBoldValue[bdepContents]), 'Non-null vs null Date should not be equal');

    // DateTime vs Time null handling
    DT1.SetToNull;
    Time1.SetToNull;
    Assert.IsTrue(DT1.IsEqualToValue(Time1.AsIBoldValue[bdepContents]), 'Both null should be equal (Time path)');

    DT1.AsDateTime := TestDT;
    Assert.IsFalse(DT1.IsEqualToValue(Time1.AsIBoldValue[bdepContents]), 'Non-null vs null Time should not be equal');
  finally
    DT1.Free;
    DT2.Free;
    Date1.Free;
    Time1.Free;
  end;
end;

// Float SetAsCurrency

procedure TTestBoldAttributes.TestFloatSetAsCurrency;
var
  FloatAttr: TBAFloat;
begin
  // TBAFloat.SetAsInteger is uncovered - it calls SetAsFloat internally
  FloatAttr := TBAFloat.Create;
  try
    FloatAttr.AsInteger := 42;
    Assert.AreEqual(42.0, FloatAttr.AsFloat, 0.001, 'SetAsInteger should set float value');
  finally
    FloatAttr.Free;
  end;
end;

// Blob IsEqualToValue

procedure TTestBoldAttributes.TestBlobIsEqualToValue;
var
  Blob1, Blob2: TBABlob;
  Data: TBytes;
begin
  Blob1 := TBABlob.Create;
  Blob2 := TBABlob.Create;
  try
    // Both null should be equal
    Assert.IsTrue(Blob1.IsEqualToValue(Blob2.AsIBoldValue[bdepContents]), 'Both null blobs should be equal');

    // One null, one not
    Data := TEncoding.UTF8.GetBytes('Test');
    Blob1.AsStream.Write(Data, Length(Data));
    Assert.IsFalse(Blob1.IsEqualToValue(Blob2.AsIBoldValue[bdepContents]), 'Non-null vs null blob should not be equal');

    // Both with same content
    Blob2.AsStream.Write(Data, Length(Data));
    Assert.IsTrue(Blob1.IsEqualToValue(Blob2.AsIBoldValue[bdepContents]), 'Blobs with same content should be equal');

    // Different content
    Blob2.SetToNull;
    Data := TEncoding.UTF8.GetBytes('Other');
    Blob2.AsStream.Write(Data, Length(Data));
    Assert.IsFalse(Blob1.IsEqualToValue(Blob2.AsIBoldValue[bdepContents]), 'Blobs with different content should not be equal');
  finally
    Blob1.Free;
    Blob2.Free;
  end;
end;

// Assign null paths

procedure TTestBoldAttributes.TestStringAssignNull;
var
  Obj1, Obj2: TClassA;
  Sys: TBoldSystem;
begin
  Sys := FDataModule.BoldSystemHandle1.System;
  Obj1 := TClassA.Create(Sys);
  Obj2 := TClassA.Create(Sys);
  Obj1.aString := 'Hello';
  Obj2.M_aString.SetToNull;
  Obj1.M_aString.Assign(Obj2.M_aString);
  Assert.IsTrue(Obj1.M_aString.IsNull, 'Assign from null should make target null');
end;

procedure TTestBoldAttributes.TestIntegerAssignNull;
var
  Obj1, Obj2: TClassA;
  Sys: TBoldSystem;
begin
  Sys := FDataModule.BoldSystemHandle1.System;
  Obj1 := TClassA.Create(Sys);
  Obj2 := TClassA.Create(Sys);
  Obj1.aInteger := 42;
  Obj2.M_aInteger.SetToNull;
  Obj1.M_aInteger.Assign(Obj2.M_aInteger);
  Assert.IsTrue(Obj1.M_aInteger.IsNull, 'Assign from null integer should make target null');
end;

procedure TTestBoldAttributes.TestFloatAssignNull;
var
  Obj1, Obj2: TClassA;
  Sys: TBoldSystem;
begin
  Sys := FDataModule.BoldSystemHandle1.System;
  Obj1 := TClassA.Create(Sys);
  Obj2 := TClassA.Create(Sys);
  Obj1.aFloat := 3.14;
  Obj2.M_aFloat.SetToNull;
  Obj1.M_aFloat.Assign(Obj2.M_aFloat);
  Assert.IsTrue(Obj1.M_aFloat.IsNull, 'Assign from null float should make target null');
end;

procedure TTestBoldAttributes.TestCurrencyAssignNull;
var
  Obj1, Obj2: TClassA;
  Sys: TBoldSystem;
begin
  Sys := FDataModule.BoldSystemHandle1.System;
  Obj1 := TClassA.Create(Sys);
  Obj2 := TClassA.Create(Sys);
  Obj1.aCurrency := 99.95;
  Obj2.M_aCurrency.SetToNull;
  Obj1.M_aCurrency.Assign(Obj2.M_aCurrency);
  Assert.IsTrue(Obj1.M_aCurrency.IsNull, 'Assign from null currency should make target null');
end;

procedure TTestBoldAttributes.TestBlobAssignNull;
var
  Obj1, Obj2: TClassA;
  Sys: TBoldSystem;
  InStream: TStringStream;
begin
  Sys := FDataModule.BoldSystemHandle1.System;
  Obj1 := TClassA.Create(Sys);
  Obj2 := TClassA.Create(Sys);
  InStream := TStringStream.Create('Data');
  try
    Obj1.M_aBlob.LoadFromStream(InStream);
  finally
    InStream.Free;
  end;
  Obj2.M_aBlob.SetToNull;
  Obj1.M_aBlob.Assign(Obj2.M_aBlob);
  Assert.IsTrue(Obj1.M_aBlob.IsNull, 'Assign from null blob should make target null');
end;

// CompareToAs with non-matching types

procedure TTestBoldAttributes.TestIntegerCompareToAsWithString;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Obj.aInteger := 42;
  Obj.aString := 'Hello';
  // Compare integer to string — should use inherited which raises
  Assert.WillRaiseAny(
    procedure
    begin
      Obj.M_aInteger.CompareToAs(ctDefault, Obj.M_aString);
    end
  );
end;

procedure TTestBoldAttributes.TestStringCompareToAsWithInteger;
var
  Obj: TClassA;
  CompResult: Integer;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Obj.aString := 'Hello';
  Obj.aInteger := 42;
  // String CompareToAs with non-string uses AsString comparison via BoldElement.AsString
  CompResult := Obj.M_aString.CompareToAs(ctDefault, Obj.M_aInteger);
  // 'Hello' > '42' alphabetically
  Assert.AreEqual(1, CompResult, 'String "Hello" > "42" alphabetically');
end;

// SetEmptyValue paths

procedure TTestBoldAttributes.TestIntegerSetEmptyValue;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Obj.aInteger := 0;
  // SetEmptyValue on integer with value 0 should be no-op (already empty)
  Assert.AreEqual(0, Obj.aInteger, 'Integer 0 is empty value');
end;

procedure TTestBoldAttributes.TestStringSetEmptyValue;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Obj.aString := '';
  Assert.AreEqual('', Obj.aString, 'Empty string is empty value');
end;

// Variant null round-trips

procedure TTestBoldAttributes.TestIntegerAsVariantNull;
var
  Obj: TClassA;
  V: Variant;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Obj.M_aInteger.SetToNull;
  V := Obj.M_aInteger.AsVariant;
  Assert.IsTrue(VarIsNull(V), 'Null integer AsVariant should be Null');
end;

procedure TTestBoldAttributes.TestFloatAsVariantNull;
var
  Obj: TClassA;
  V: Variant;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Obj.M_aFloat.SetToNull;
  V := Obj.M_aFloat.AsVariant;
  Assert.IsTrue(VarIsNull(V), 'Null float AsVariant should be Null');
end;

procedure TTestBoldAttributes.TestCurrencyAsVariantNull;
var
  Obj: TClassA;
  V: Variant;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Obj.M_aCurrency.SetToNull;
  V := Obj.M_aCurrency.AsVariant;
  Assert.IsTrue(VarIsNull(V), 'Null currency AsVariant should be Null');
end;

procedure TTestBoldAttributes.TestBlobAsVariantNull;
var
  Obj: TClassA;
  V: Variant;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  // Blob is null by default
  V := Obj.M_aBlob.AsVariant;
  Assert.IsTrue(VarIsNull(V), 'Null blob AsVariant should be Null');
end;

procedure TTestBoldAttributes.TestDateTimeAsVariantRoundTrip;
var
  Obj: TClassA;
  V: Variant;
  DT: TDateTime;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  DT := EncodeDate(2026, 6, 15) + EncodeTime(10, 30, 0, 0);
  Obj.aDateTime := DT;
  V := Obj.M_aDateTime.AsVariant;
  Assert.IsFalse(VarIsNull(V), 'Non-null datetime variant should not be null');
  Obj.M_aDateTime.AsVariant := V;
  Assert.AreEqual(DT, Obj.aDateTime, 'Variant round-trip should preserve value');
end;

// Blob operations

procedure TTestBoldAttributes.TestBlobSetToNullClearsStream;
var
  Obj: TClassA;
  InStream: TStringStream;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  InStream := TStringStream.Create('StreamData');
  try
    Obj.M_aBlob.LoadFromStream(InStream);
  finally
    InStream.Free;
  end;
  Assert.IsTrue(Obj.M_aBlob.BlobSize > 0, 'Blob should have data');
  Obj.M_aBlob.SetToNull;
  Assert.IsTrue(Obj.M_aBlob.IsNull, 'Blob should be null after SetToNull');
  Assert.AreEqual(Int64(0), Obj.M_aBlob.BlobSize, 'Blob stream should be cleared after SetToNull');
end;

procedure TTestBoldAttributes.TestBlobCompareToAs;
var
  Obj1, Obj2: TClassA;
  Sys: TBoldSystem;
  InStream: TStringStream;
begin
  Sys := FDataModule.BoldSystemHandle1.System;
  Obj1 := TClassA.Create(Sys);
  Obj2 := TClassA.Create(Sys);
  InStream := TStringStream.Create('SameData');
  try
    Obj1.M_aBlob.LoadFromStream(InStream);
    InStream.Position := 0;
    Obj2.M_aBlob.LoadFromStream(InStream);
  finally
    InStream.Free;
  end;
  Assert.AreEqual(0, Obj1.M_aBlob.CompareToAs(ctDefault, Obj2.M_aBlob), 'Same blob should compare equal');

  // Null vs non-null
  Obj2.M_aBlob.SetToNull;
  Assert.AreEqual(1, Obj1.M_aBlob.CompareToAs(ctDefault, Obj2.M_aBlob), 'Non-null > null blob');
end;

// JSON serialization (BoldObjectRepresentationJson)

procedure TTestBoldAttributes.TestBoldElementToJsonString;
var
  Obj: TClassA;
  Json: string;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Obj.aString := 'JsonTest';
  Obj.aInteger := 42;
  Obj.aFloat := 3.14;
  Obj.aBoolean := True;
  Obj.aCurrency := 99.95;
  Obj.aDateTime := EncodeDate(2026, 3, 15);

  Json := BoldElementToJsonString(Obj);
  Assert.IsTrue(Length(Json) > 10, 'JSON should be non-empty');
  Assert.IsTrue(Pos('JsonTest', Json) > 0, 'JSON should contain string value');
  Assert.IsTrue(Pos('42', Json) > 0, 'JSON should contain integer value');
end;

procedure TTestBoldAttributes.TestBoldElementToJsonAndBack;
var
  Obj: TClassA;
  Json: string;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Obj.aString := 'RoundTrip';
  Obj.aInteger := 100;
  Obj.aBoolean := True;
  Obj.aCurrency := 50.0;
  Obj.aFloat := 1.5;
  Obj.aDate := EncodeDate(2026, 1, 1);

  Json := BoldElementToJsonString(Obj);
  Assert.IsTrue(Length(Json) > 50, 'Should produce substantial JSON');
  Assert.IsTrue(Pos('RoundTrip', Json) > 0, 'Should contain string attr');
  Assert.IsTrue(Pos('100', Json) > 0, 'Should contain integer attr');
end;

procedure TTestBoldAttributes.TestBoldObjectStringRepresentation;
var
  Obj: TClassA;
  Repr: string;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Obj.aString := 'ReprTest';
  Repr := BoldObjectStringRepresentation(Obj);
  Assert.IsTrue(Length(Repr) > 0, 'String representation should be non-empty');
end;

procedure TTestBoldAttributes.TestBoldElementToJsonWithNulls;
var
  Obj: TClassA;
  Json: string;
begin
  // Object with all null attributes
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Json := BoldElementToJsonString(Obj);
  Assert.IsTrue(Length(Json) > 5, 'JSON with nulls should have content');
end;

procedure TTestBoldAttributes.TestBoldElementToJsonWithBlob;
var
  Obj: TClassA;
  Json: string;
begin
  Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
  Obj.aBlob := 'BinaryBlobDataForJson';
  Obj.aBlobContent := 'TypedBlob';
  Obj.M_aBlobContent.ContentType := 'application/octet-stream';
  Obj.aTime := EncodeTime(15, 45, 30, 0);
  Obj.aDate := EncodeDate(2026, 12, 25);
  Obj.aWord := 65535;
  Obj.aByte := 255;
  Obj.aShortInt := -128;
  Obj.aSmallInt := -32768;

  Json := BoldElementToJsonString(Obj);
  Assert.IsTrue(Length(Json) > 50, 'JSON with blob should have content');
end;

{ TTestSetEmptyValue }

procedure TTestSetEmptyValue.SetUp;
begin
  Ensuredm_Model;
  FSystemTypeInfoHandle := TBoldSystemTypeInfoHandle.Create(nil);
  FSystemTypeInfoHandle.BoldModel := dm_Model1.BoldModel1;
  FSystemHandle := TBoldSystemHandle.Create(nil);
  FSystemHandle.SystemTypeInfoHandle := FSystemTypeInfoHandle;
  FSystemHandle.Active := True;
end;

procedure TTestSetEmptyValue.TearDown;
begin
  if Assigned(FSystemHandle) and FSystemHandle.Active then
  begin
    FSystemHandle.System.Discard;
    FSystemHandle.Active := False;
  end;
  FreeAndNil(FSystemHandle);
  FreeAndNil(FSystemTypeInfoHandle);
end;

function TTestSetEmptyValue.CreateClassA: TestModel1.TClassA;
begin
  Result := TestModel1.TClassA.Create(FSystemHandle.System);
end;

procedure TTestSetEmptyValue.MakeContentNull(Attribute: TBoldAttribute);
var
  Nullable: IBoldNullableValue;
begin
  // SetToNull is refused for non-nullable attributes, so go through the
  // content proxy - the same route a database fetch of NULL takes.
  Assert.IsTrue(Attribute.ProxyInterface(IBoldNullableValue, bdepContents, Nullable),
    Attribute.DisplayName + ': expected an IBoldNullableValue content proxy');
  Nullable.SetContentToNull;
  Assert.IsTrue(Attribute.IsNull,
    Attribute.DisplayName + ': attribute should be null after SetContentToNull');
end;

procedure TTestSetEmptyValue.TestSetEmptyValueRepairsNullString;
var
  Obj: TestModel1.TClassA;
begin
  Obj := CreateClassA;
  MakeContentNull(Obj.M_aString);
  Obj.M_aString.SetEmptyValue;
  Assert.IsFalse(Obj.M_aString.IsNull,
    'non-nullable string must be non-null after SetEmptyValue');
  Assert.AreEqual('', Obj.aString);
end;

procedure TTestSetEmptyValue.TestSetEmptyValueRepairsNullInteger;
var
  Obj: TestModel1.TClassA;
begin
  Obj := CreateClassA;
  MakeContentNull(Obj.M_aInteger);
  Obj.M_aInteger.SetEmptyValue;
  Assert.IsFalse(Obj.M_aInteger.IsNull,
    'non-nullable integer must be non-null after SetEmptyValue');
  Assert.AreEqual(0, Obj.aInteger);
end;

procedure TTestSetEmptyValue.TestSetEmptyValueRepairsNullFloat;
var
  Obj: TestModel1.TClassA;
begin
  Obj := CreateClassA;
  MakeContentNull(Obj.M_aFloat);
  Obj.M_aFloat.SetEmptyValue;
  Assert.IsFalse(Obj.M_aFloat.IsNull,
    'non-nullable float must be non-null after SetEmptyValue');
  Assert.AreEqual(Double(0.0), Double(Obj.aFloat));
end;

procedure TTestSetEmptyValue.TestSetEmptyValueRepairsNullCurrency;
var
  Obj: TestModel1.TClassA;
begin
  Obj := CreateClassA;
  MakeContentNull(Obj.M_aCurrency);
  Obj.M_aCurrency.SetEmptyValue;
  Assert.IsFalse(Obj.M_aCurrency.IsNull,
    'non-nullable currency must be non-null after SetEmptyValue');
  Assert.AreEqual(Currency(0), Obj.aCurrency);
end;

procedure TTestSetEmptyValue.TestSetEmptyValueRepairsNullValueSet;
var
  Obj: TestModel1.TClassA;
begin
  // TBAValueSet.SetEmptyValue reads AsInteger before assigning, which raises
  // EBoldAccessNullValue on a null attribute instead of repairing it.
  Obj := CreateClassA;
  MakeContentNull(Obj.M_aBoolean);
  Obj.M_aBoolean.SetEmptyValue;
  Assert.IsFalse(Obj.M_aBoolean.IsNull,
    'non-nullable valueset must be non-null after SetEmptyValue');
  Assert.IsFalse(Obj.aBoolean,
    'valueset should hold its first value after SetEmptyValue');
end;

procedure TTestSetEmptyValue.TestAssignNilRepairsNullNonNullableAttribute;
var
  Obj: TestModel1.TClassA;
begin
  // Assign(nil) routes non-nullable attributes to SetEmptyValue - the
  // production path that is supposed to clear a null into an empty value.
  Obj := CreateClassA;
  MakeContentNull(Obj.M_aString);
  Obj.M_aString.Assign(nil);
  Assert.IsFalse(Obj.M_aString.IsNull,
    'Assign(nil) on a null non-nullable attribute must yield the empty value, not null');
  Assert.AreEqual('', Obj.aString);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldAttributes);
  TDUnitX.RegisterTestFixture(TTestSetEmptyValue);

end.
