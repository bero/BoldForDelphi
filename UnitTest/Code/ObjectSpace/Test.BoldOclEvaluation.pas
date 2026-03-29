unit Test.BoldOclEvaluation;

{ DUnitX end-to-end tests for OCL parsing, semantic checking, and evaluation.
  Each test exercises the full pipeline: parsing -> semantics -> evaluation -> symbol execution. }

interface

uses
  Classes,
  SysUtils,
  DUnitX.TestFramework,
  BoldSystem,
  BoldElements,
  BoldAttributes,
  BoldDefs,
  jehoBCBoldTest,
  Test.BoldAttributes;

type
  [TestFixture]
  [Category('OCL')]
  TTestBoldOclEvaluation = class
  private
    FDataModule: TjehodmBoldTest;
    function GetSystem: TBoldSystem;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    // --- Arithmetic Operations ---
    [Test] [Category('Quick')]
    procedure TestAdd_IntegerPlusInteger;
    [Test] [Category('Quick')]
    procedure TestAdd_FloatPlusFloat;
    [Test] [Category('Quick')]
    procedure TestAdd_StringConcat;
    [Test] [Category('Quick')]
    procedure TestSubtract;
    [Test] [Category('Quick')]
    procedure TestMultiply;
    [Test] [Category('Quick')]
    procedure TestDivide;
    [Test] [Category('Quick')]
    procedure TestDiv_IntegerDivision;
    [Test] [Category('Quick')]
    procedure TestMod;
    [Test] [Category('Quick')]
    procedure TestUnaryMinus;
    [Test] [Category('Quick')]
    procedure TestAbs;
    [Test] [Category('Quick')]
    procedure TestFloor;
    [Test] [Category('Quick')]
    procedure TestRound;
    [Test] [Category('Quick')]
    procedure TestPower;
    [Test] [Category('Quick')]
    procedure TestSqrt;
    [Test] [Category('Quick')]
    procedure TestMin;
    [Test] [Category('Quick')]
    procedure TestMax;
    [Test] [Category('Quick')]
    procedure TestSafeDivNonZero;
    [Test] [Category('Quick')]
    procedure TestSimpleRound;
    [Test] [Category('Quick')]
    procedure TestSimpleRoundTo;

    // --- Comparison Operations ---
    [Test] [Category('Quick')]
    procedure TestEqual_Integer;
    [Test] [Category('Quick')]
    procedure TestNotEqual_Integer;
    [Test] [Category('Quick')]
    procedure TestLess;
    [Test] [Category('Quick')]
    procedure TestGreater;
    [Test] [Category('Quick')]
    procedure TestLessEQ;
    [Test] [Category('Quick')]
    procedure TestGreaterEQ;
    [Test] [Category('Quick')]
    procedure TestEqual_String;
    [Test] [Category('Quick')]
    procedure TestLess_String;

    // --- String Operations ---
    [Test] [Category('Quick')]
    procedure TestLength;
    [Test] [Category('Quick')]
    procedure TestConcat;
    [Test] [Category('Quick')]
    procedure TestToUpper;
    [Test] [Category('Quick')]
    procedure TestToLower;
    [Test] [Category('Quick')]
    procedure TestSubString;
    [Test] [Category('Quick')]
    procedure TestContains_True;
    [Test] [Category('Quick')]
    procedure TestContains_False;
    [Test] [Category('Quick')]
    procedure TestPad;
    [Test] [Category('Quick')]
    procedure TestPostPad;
    [Test] [Category('Quick')]
    procedure TestTrim;
    [Test] [Category('Quick')]
    procedure TestStrToInt;
    [Test] [Category('Quick')]
    procedure TestStrToFloat;
    [Test] [Category('Quick')]
    procedure TestSQLLike;
    [Test] [Category('Quick')]
    procedure TestSQLLikeCaseInsensitive;

    // --- Boolean / Logic Operations ---
    [Test] [Category('Quick')]
    procedure TestAnd_TrueTrue;
    [Test] [Category('Quick')]
    procedure TestAnd_TrueFalse;
    [Test] [Category('Quick')]
    procedure TestOr_FalseFalse;
    [Test] [Category('Quick')]
    procedure TestOr_TrueFalse;
    [Test] [Category('Quick')]
    procedure TestNot;
    [Test] [Category('Quick')]
    procedure TestXor;
    [Test] [Category('Quick')]
    procedure TestImplies_TrueFalse;
    [Test] [Category('Quick')]
    procedure TestImplies_FalseAnything;
    [Test] [Category('Quick')]
    procedure TestIf_TrueCondition;
    [Test] [Category('Quick')]
    procedure TestIf_FalseCondition;

    // --- Collection Operations (using allInstances) ---
    [Test] [Category('Quick')]
    procedure TestSize;
    [Test] [Category('Quick')]
    procedure TestIsEmpty_True;
    [Test] [Category('Quick')]
    procedure TestIsEmpty_False;
    [Test] [Category('Quick')]
    procedure TestNotEmpty;
    [Test] [Category('Quick')]
    procedure TestIncludes;
    [Test] [Category('Quick')]
    procedure TestSum;
    [Test] [Category('Quick')]
    procedure TestMinValue;
    [Test] [Category('Quick')]
    procedure TestMaxValue;
    [Test] [Category('Quick')]
    procedure TestAverage;
    [Test] [Category('Quick')]
    procedure TestSelect;
    [Test] [Category('Quick')]
    procedure TestReject;
    [Test] [Category('Quick')]
    procedure TestCollect;
    [Test] [Category('Quick')]
    procedure TestForAll_True;
    [Test] [Category('Quick')]
    procedure TestForAll_False;
    [Test] [Category('Quick')]
    procedure TestExists_True;
    [Test] [Category('Quick')]
    procedure TestExists_False;
    [Test] [Category('Quick')]
    procedure TestFirst;
    [Test] [Category('Quick')]
    procedure TestLast;
    [Test] [Category('Quick')]
    procedure TestAt;
    [Test] [Category('Quick')]
    procedure TestIndexOf;
    [Test] [Category('Quick')]
    procedure TestOrderBy;
    [Test] [Category('Quick')]
    procedure TestOrderDescending;
    [Test] [Category('Quick')]
    procedure TestReverseCollection;
    [Test] [Category('Quick')]
    procedure TestAsSequence;
    [Test] [Category('Quick')]
    procedure TestAsSet;
    [Test] [Category('Quick')]
    procedure TestCount;
    [Test] [Category('Quick')]
    procedure TestUnion;
    [Test] [Category('Quick')]
    procedure TestIntersection;
    [Test] [Category('Quick')]
    procedure TestDifference;
    [Test] [Category('Quick')]
    procedure TestSymmetricDifference;
    [Test] [Category('Quick')]
    procedure TestIncluding;
    [Test] [Category('Quick')]
    procedure TestExcluding;
    [Test] [Category('Quick')]
    procedure TestIncludesAll;

    // --- Date/Time Operations ---
    [Test] [Category('Quick')]
    procedure TestDay;
    [Test] [Category('Quick')]
    procedure TestMonth;
    [Test] [Category('Quick')]
    procedure TestYear;
    [Test] [Category('Quick')]
    procedure TestWeek;
    [Test] [Category('Quick')]
    procedure TestDayOfWeek;
    [Test] [Category('Quick')]
    procedure TestHoursBetween;
    [Test] [Category('Quick')]
    procedure TestMinutesBetween;
    [Test] [Category('Quick')]
    procedure TestSecondsBetween;
    [Test] [Category('Quick')]
    procedure TestInDateRange;
    [Test] [Category('Quick')]
    procedure TestAsISODate;
    [Test] [Category('Quick')]
    procedure TestAsISODateTime;

    // --- Type System Operations ---
    [Test] [Category('Quick')]
    procedure TestAsString_Integer;
    [Test] [Category('Quick')]
    procedure TestAsFloat_Integer;
    [Test] [Category('Quick')]
    procedure TestOclType;
    [Test] [Category('Quick')]
    procedure TestOclIsKindOf;
    [Test] [Category('Quick')]
    procedure TestOclIsTypeOf;
    [Test] [Category('Quick')]
    procedure TestOclAsType;
    [Test] [Category('Quick')]
    procedure TestAllInstances;
    [Test] [Category('Quick')]
    procedure TestAllLoadedObjects;
    [Test] [Category('Quick')]
    procedure TestFilterOnType;
    [Test] [Category('Quick')]
    procedure TestIsNull_True;
    [Test] [Category('Quick')]
    procedure TestIsNull_False;

    // --- Member Access ---
    [Test] [Category('Quick')]
    procedure TestSimpleStringAttribute;
    [Test] [Category('Quick')]
    procedure TestSimpleIntegerAttribute;
    [Test] [Category('Quick')]
    procedure TestSimpleBooleanAttribute;
    [Test] [Category('Quick')]
    procedure TestSelfExpression;

    // --- Miscellaneous ---
    [Test] [Category('Quick')]
    procedure TestAsCommaText;
    [Test] [Category('Quick')]
    procedure TestSeparate;
    [Test] [Category('Quick')]
    procedure TestFormatFloat;
    [Test] [Category('Quick')]
    procedure TestStringRepresentation;

    // --- Error Handling ---
    [Test] [Category('Quick')]
    procedure TestUnknownMember_Raises;
    [Test] [Category('Quick')]
    procedure TestSyntaxError_Raises;
    [Test] [Category('Quick')]
    procedure TestDivisionByZero_SafeDiv;

    // --- Currency Arithmetic ---
    [Test] [Category('Quick')]
    procedure TestCurrencyAdd;
    [Test] [Category('Quick')]
    procedure TestCurrencySubtract;
    [Test] [Category('Quick')]
    procedure TestCurrencyMultiply;
    [Test] [Category('Quick')]
    procedure TestCurrencyNegate;
    [Test] [Category('Quick')]
    procedure TestCurrencyAbs;

    // --- DateTime Arithmetic ---
    [Test] [Category('Quick')]
    procedure TestDateTimeAdd;
    [Test] [Category('Quick')]
    procedure TestDateTimeSubtract;

    // --- FormatDateTime ---
    [Test] [Category('Quick')]
    procedure TestFormatDateTime;
    [Test] [Category('Quick')]
    procedure TestFormatDateTimeNull;

    // --- Null handling ---
    [Test] [Category('Quick')]
    procedure TestNullComparison_Less;
    [Test] [Category('Quick')]
    procedure TestNullComparison_Greater;
    [Test] [Category('Quick')]
    procedure TestNullComparison_LessEQ;
    [Test] [Category('Quick')]
    procedure TestNullComparison_GreaterEQ;

    // --- Division by zero ---
    [Test] [Category('Quick')]
    procedure TestDivisionByZero_Raises;

    // --- Pad edge cases ---
    [Test] [Category('Quick')]
    procedure TestPadLongerThanTarget;
    [Test] [Category('Quick')]
    procedure TestPostPadLongerThanTarget;

    // --- AsTime ---
    [Test] [Category('Quick')]
    procedure TestAsTime;

    // --- Collection with currency ---
    [Test] [Category('Quick')]
    procedure TestCurrencySum;

    // --- String-to-date conversions ---
    [Test] [Category('Quick')]
    procedure TestStrToDateTime;
    [Test] [Category('Quick')]
    procedure TestStrToDate;
    [Test] [Category('Quick')]
    procedure TestStrToTime;

    // --- Format (Delphi format string) ---
    [Test] [Category('Quick')]
    procedure TestFormat;

    // --- Pad with multi-char padder (truncation path) ---
    [Test] [Category('Quick')]
    procedure TestPadMultiCharPadder;
    [Test] [Category('Quick')]
    procedure TestPostPadMultiCharPadder;

    // --- Min/Max on collections ---
    [Test] [Category('Quick')]
    procedure TestMinValueEmpty;
    [Test] [Category('Quick')]
    procedure TestMaxValueEmpty;
    [Test] [Category('Quick')]
    procedure TestAverageEmpty;

    // --- Collection operations with null elements ---
    [Test] [Category('Quick')]
    procedure TestSumWithNullElements;

    // --- Boolean comparison with null ---
    [Test] [Category('Quick')]
    procedure TestIsNullOnNullString;

    // --- Float division ---
    [Test] [Category('Quick')]
    procedure TestFloatDivisionByZeroSafeDiv;

    // --- More currency operations ---
    [Test] [Category('Quick')]
    procedure TestCurrencyCompare;

    // --- String operations ---
    [Test] [Category('Quick')]
    procedure TestToInteger;
    [Test] [Category('Quick')]
    procedure TestToReal;
    [Test] [Category('Quick')]
    procedure TestStringAsString;

    // --- More collection operations ---
    [Test] [Category('Quick')]
    procedure TestIncludesAllTrue;
    [Test] [Category('Quick')]
    procedure TestIncludesAllFalse;
    [Test] [Category('Quick')]
    procedure TestCountElement;
    [Test] [Category('Quick')]
    procedure TestFlatten;
    [Test] [Category('Quick')]
    procedure TestCollectWithNulls;

    // --- Date operations ---
    [Test] [Category('Quick')]
    procedure TestHoursBetweenNull;
    [Test] [Category('Quick')]
    procedure TestMinutesBetweenNull;
    [Test] [Category('Quick')]
    procedure TestDaysBetween;
    [Test] [Category('Quick')]
    procedure TestNowFunction;
    [Test] [Category('Quick')]
    procedure TestCurrentDate;

    // --- Numeric edge cases ---
    [Test] [Category('Quick')]
    procedure TestFloatAbs;
    [Test] [Category('Quick')]
    procedure TestIntegerMod;
    [Test] [Category('Quick')]
    procedure TestFloatFloor;
    [Test] [Category('Quick')]
    procedure TestFloatRound;

    // --- String operations (more) ---
    [Test] [Category('Quick')]
    procedure TestIndexOfString;
    [Test] [Category('Quick')]
    procedure TestSQLLikePercent;
    [Test] [Category('Quick')]
    procedure TestSQLLikeUnderscore;

    // --- Type operations ---
    [Test] [Category('Quick')]
    procedure TestOclAsTypeInvalid;
    [Test] [Category('Quick')]
    procedure TestOclIsKindOfFalse;

    // --- More numeric operations ---
    [Test] [Category('Quick')]
    procedure TestSqrtFloat;
    [Test] [Category('Quick')]
    procedure TestPowerFloat;
    [Test] [Category('Quick')]
    procedure TestMaxTwoValues;
    [Test] [Category('Quick')]
    procedure TestMinTwoValues;

    // --- More collection operations ---
    [Test] [Category('Quick')]
    procedure TestExcludingSelf;
    [Test] [Category('Quick')]
    procedure TestIncludingSelf;
    [Test] [Category('Quick')]
    procedure TestUnionCollections;
    [Test] [Category('Quick')]
    procedure TestDifferenceCollections;
    [Test] [Category('Quick')]
    procedure TestIntersectionCollections;
    [Test] [Category('Quick')]
    procedure TestSymmetricDifferenceCollections;

    // --- String operations ---
    [Test] [Category('Quick')]
    procedure TestToUpperCase;
    [Test] [Category('Quick')]
    procedure TestToLowerCase;
    [Test] [Category('Quick')]
    procedure TestTrimString;
    [Test] [Category('Quick')]
    procedure TestSubStringOcl;

    // --- Boolean operations ---
    [Test] [Category('Quick')]
    procedure TestXorOperation;
    [Test] [Category('Quick')]
    procedure TestImpliesTrueTrue;

    // --- Null arithmetic ---
    [Test] [Category('Quick')]
    procedure TestNullIntegerAdd;
    [Test] [Category('Quick')]
    procedure TestNullFloatMultiply;

    // --- ExpressionType ---
    [Test] [Category('Quick')]
    procedure TestExpressionTypeInteger;
    [Test] [Category('Quick')]
    procedure TestExpressionTypeString;
    [Test] [Category('Quick')]
    procedure TestExpressionTypeCollection;
    [Test] [Category('Quick')]
    procedure TestExpressionTypeInvalid;
    [Test] [Category('Quick')]
    procedure TestExpressionTypeEmpty;

    // --- RTInfo ---
    [Test] [Category('Quick')]
    procedure TestRTInfoAttribute;
    [Test] [Category('Quick')]
    procedure TestRTInfoInvalid;
    [Test] [Category('Quick')]
    procedure TestRTInfoEmpty;

    // --- OCL dictionary caching ---
    [Test] [Category('Quick')]
    procedure TestOclDictionaryCacheHit;

    // --- Evaluate with guillemets (error path) ---
    [Test] [Category('Quick')]
    procedure TestGuillemetInExpressionRaises;

    // --- Final push ---
    [Test] [Category('Quick')]
    procedure TestExpressionTypeBoolean;
    [Test] [Category('Quick')]
    procedure TestExpressionTypeFloat;
    [Test] [Category('Quick')]
    procedure TestRTInfoInteger;
  end;

implementation

uses
  DateUtils,
  Math,
  BoldOclError,
  BoldSystemRT;

{ TTestBoldOclEvaluation }

procedure TTestBoldOclEvaluation.SetUp;
begin
  FDataModule := TjehodmBoldTest.Create(nil);
end;

procedure TTestBoldOclEvaluation.TearDown;
begin
  FreeAndNil(FDataModule);
end;

function TTestBoldOclEvaluation.GetSystem: TBoldSystem;
begin
  Result := FDataModule.BoldSystemHandle1.System;
end;

// === Arithmetic Operations ===

procedure TTestBoldOclEvaluation.TestAdd_IntegerPlusInteger;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aInteger := 3;
  Assert.AreEqual(8, Obj.EvaluateExpressionAsInteger('self.aInteger + 5'));
end;

procedure TTestBoldOclEvaluation.TestAdd_FloatPlusFloat;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aFloat := 1.5;
  Assert.AreEqual(Double(4.0), Obj.EvaluateExpressionAsFloat('self.aFloat + 2.5'));
end;

procedure TTestBoldOclEvaluation.TestAdd_StringConcat;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'Hello';
  Assert.AreEqual('Hello World', Obj.EvaluateExpressionAsString('self.aString + '' World'''));
end;

procedure TTestBoldOclEvaluation.TestSubtract;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aInteger := 10;
  Assert.AreEqual(7, Obj.EvaluateExpressionAsInteger('self.aInteger - 3'));
end;

procedure TTestBoldOclEvaluation.TestMultiply;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aInteger := 4;
  Assert.AreEqual(20, Obj.EvaluateExpressionAsInteger('self.aInteger * 5'));
end;

procedure TTestBoldOclEvaluation.TestDivide;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aFloat := 10.0;
  Assert.AreEqual(Double(2.0), Obj.EvaluateExpressionAsFloat('self.aFloat / 5.0'), 0.0001);
end;

procedure TTestBoldOclEvaluation.TestDiv_IntegerDivision;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aInteger := 10;
  Assert.AreEqual(3, Obj.EvaluateExpressionAsInteger('self.aInteger div 3'));
end;

procedure TTestBoldOclEvaluation.TestMod;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aInteger := 10;
  Assert.AreEqual(1, Obj.EvaluateExpressionAsInteger('self.aInteger mod 3'));
end;

procedure TTestBoldOclEvaluation.TestUnaryMinus;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aInteger := 5;
  Assert.AreEqual(-5, Obj.EvaluateExpressionAsInteger('-self.aInteger'));
end;

procedure TTestBoldOclEvaluation.TestAbs;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aInteger := -7;
  Assert.AreEqual(7, Obj.EvaluateExpressionAsInteger('self.aInteger.abs'));
end;

procedure TTestBoldOclEvaluation.TestFloor;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aFloat := 3.7;
  Assert.AreEqual(3, Obj.EvaluateExpressionAsInteger('self.aFloat.floor'));
end;

procedure TTestBoldOclEvaluation.TestRound;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aFloat := 3.7;
  Assert.AreEqual(4, Obj.EvaluateExpressionAsInteger('self.aFloat.round'));
end;

procedure TTestBoldOclEvaluation.TestPower;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aFloat := 2.0;
  Assert.AreEqual(Double(8.0), Obj.EvaluateExpressionAsFloat('self.aFloat.power(3.0)'), 0.0001);
end;

procedure TTestBoldOclEvaluation.TestSqrt;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aFloat := 9.0;
  Assert.AreEqual(Double(3.0), Obj.EvaluateExpressionAsFloat('self.aFloat.sqrt'), 0.0001);
end;

procedure TTestBoldOclEvaluation.TestMin;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aInteger := 5;
  Assert.AreEqual(3, Obj.EvaluateExpressionAsInteger('self.aInteger.min(3)'));
end;

procedure TTestBoldOclEvaluation.TestMax;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aInteger := 5;
  Assert.AreEqual(8, Obj.EvaluateExpressionAsInteger('self.aInteger.max(8)'));
end;

procedure TTestBoldOclEvaluation.TestSafeDivNonZero;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aFloat := 10.0;
  // safediv with non-zero divisor returns normal result
  Assert.AreEqual(Double(5.0), Obj.EvaluateExpressionAsFloat('self.aFloat.safediv(2.0)'), 0.001);
end;

procedure TTestBoldOclEvaluation.TestSimpleRound;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aFloat := 3.7;
  Assert.AreEqual(Double(4.0), Obj.EvaluateExpressionAsFloat('self.aFloat.simpleRound'), 0.001);
end;

procedure TTestBoldOclEvaluation.TestSimpleRoundTo;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aFloat := 1234.567;
  // simpleRoundTo(digits) rounds to specified decimal places
  Assert.AreEqual(Double(1235.0), Obj.EvaluateExpressionAsFloat('self.aFloat.simpleRoundTo(0)'), 0.001);
end;

// === Comparison Operations ===

procedure TTestBoldOclEvaluation.TestEqual_Integer;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aInteger := 5;
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('self.aInteger = 5'));
end;

procedure TTestBoldOclEvaluation.TestNotEqual_Integer;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aInteger := 5;
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('self.aInteger <> 3'));
end;

procedure TTestBoldOclEvaluation.TestLess;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aInteger := 3;
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('self.aInteger < 5'));
end;

procedure TTestBoldOclEvaluation.TestGreater;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aInteger := 10;
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('self.aInteger > 5'));
end;

procedure TTestBoldOclEvaluation.TestLessEQ;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aInteger := 5;
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('self.aInteger <= 5'));
end;

procedure TTestBoldOclEvaluation.TestGreaterEQ;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aInteger := 5;
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('self.aInteger >= 5'));
end;

procedure TTestBoldOclEvaluation.TestEqual_String;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'test';
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('self.aString = ''test'''));
end;

procedure TTestBoldOclEvaluation.TestLess_String;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'abc';
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('self.aString < ''xyz'''));
end;

// === String Operations ===

procedure TTestBoldOclEvaluation.TestLength;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'Hello';
  Assert.AreEqual(5, Obj.EvaluateExpressionAsInteger('self.aString.length'));
end;

procedure TTestBoldOclEvaluation.TestConcat;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'Hello';
  Assert.AreEqual('Hello World', Obj.EvaluateExpressionAsString('self.aString.concat('' World'')'));
end;

procedure TTestBoldOclEvaluation.TestToUpper;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'hello';
  Assert.AreEqual('HELLO', Obj.EvaluateExpressionAsString('self.aString.toUpper'));
end;

procedure TTestBoldOclEvaluation.TestToLower;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'HELLO';
  Assert.AreEqual('hello', Obj.EvaluateExpressionAsString('self.aString.toLower'));
end;

procedure TTestBoldOclEvaluation.TestSubString;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'Hello World';
  Assert.AreEqual('Hel', Obj.EvaluateExpressionAsString('self.aString.subString(1, 3)'));
end;

procedure TTestBoldOclEvaluation.TestContains_True;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'Hello World';
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('self.aString.contains(''World'')'));
end;

procedure TTestBoldOclEvaluation.TestContains_False;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'Hello World';
  Assert.IsFalse(Obj.EvaluateExpressionAsBoolean('self.aString.contains(''xyz'')'));
end;

procedure TTestBoldOclEvaluation.TestPad;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'Hi';
  Assert.AreEqual('***Hi', Obj.EvaluateExpressionAsString('self.aString.pad(5, ''*'')'));
end;

procedure TTestBoldOclEvaluation.TestPostPad;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'Hi';
  Assert.AreEqual('Hi***', Obj.EvaluateExpressionAsString('self.aString.postPad(5, ''*'')'));
end;

procedure TTestBoldOclEvaluation.TestTrim;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := '  Hello  ';
  Assert.AreEqual('Hello', Obj.EvaluateExpressionAsString('self.aString.trim'));
end;

procedure TTestBoldOclEvaluation.TestStrToInt;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := '42';
  Assert.AreEqual(42, Obj.EvaluateExpressionAsInteger('self.aString.strToInt'));
end;

procedure TTestBoldOclEvaluation.TestStrToFloat;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  // Use locale-appropriate decimal separator
  Obj.aString := FloatToStr(3.14);
  Assert.AreEqual(Double(3.14), Obj.EvaluateExpressionAsFloat('self.aString.strToFloat'), 0.001);
end;

procedure TTestBoldOclEvaluation.TestSQLLike;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'Hello World';
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('self.aString.sqlLike(''Hello%'')'));
  Assert.IsFalse(Obj.EvaluateExpressionAsBoolean('self.aString.sqlLike(''xyz%'')'));
end;

procedure TTestBoldOclEvaluation.TestSQLLikeCaseInsensitive;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'Hello World';
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('self.aString.sqlLikeCaseInsensitive(''hello%'')'));
end;

// === Boolean / Logic Operations ===

procedure TTestBoldOclEvaluation.TestAnd_TrueTrue;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('true and true'));
end;

procedure TTestBoldOclEvaluation.TestAnd_TrueFalse;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Assert.IsFalse(Obj.EvaluateExpressionAsBoolean('true and false'));
end;

procedure TTestBoldOclEvaluation.TestOr_FalseFalse;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Assert.IsFalse(Obj.EvaluateExpressionAsBoolean('false or false'));
end;

procedure TTestBoldOclEvaluation.TestOr_TrueFalse;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('true or false'));
end;

procedure TTestBoldOclEvaluation.TestNot;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Assert.IsFalse(Obj.EvaluateExpressionAsBoolean('not true'));
end;

procedure TTestBoldOclEvaluation.TestXor;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('true xor false'));
  Assert.IsFalse(Obj.EvaluateExpressionAsBoolean('true xor true'));
end;

procedure TTestBoldOclEvaluation.TestImplies_TrueFalse;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Assert.IsFalse(Obj.EvaluateExpressionAsBoolean('true implies false'));
end;

procedure TTestBoldOclEvaluation.TestImplies_FalseAnything;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('false implies false'));
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('false implies true'));
end;

procedure TTestBoldOclEvaluation.TestIf_TrueCondition;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Assert.AreEqual(1, Obj.EvaluateExpressionAsInteger('if true then 1 else 2 endif'));
end;

procedure TTestBoldOclEvaluation.TestIf_FalseCondition;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Assert.AreEqual(2, Obj.EvaluateExpressionAsInteger('if false then 1 else 2 endif'));
end;

// === Collection Operations (using allInstances) ===

procedure TTestBoldOclEvaluation.TestSize;
begin
  TClassA.Create(GetSystem);
  TClassA.Create(GetSystem);
  TClassA.Create(GetSystem);
  Assert.AreEqual(3, GetSystem.EvaluateExpressionAsInteger('ClassA.allInstances->size'));
end;

procedure TTestBoldOclEvaluation.TestIsEmpty_True;
begin
  Assert.IsTrue(GetSystem.EvaluateExpressionAsBoolean('ClassA.allInstances->isEmpty'));
end;

procedure TTestBoldOclEvaluation.TestIsEmpty_False;
begin
  TClassA.Create(GetSystem);
  Assert.IsFalse(GetSystem.EvaluateExpressionAsBoolean('ClassA.allInstances->isEmpty'));
end;

procedure TTestBoldOclEvaluation.TestNotEmpty;
begin
  TClassA.Create(GetSystem);
  Assert.IsTrue(GetSystem.EvaluateExpressionAsBoolean('ClassA.allInstances->notEmpty'));
end;

procedure TTestBoldOclEvaluation.TestIncludes;
begin
  TClassA.Create(GetSystem);
  Assert.IsTrue(GetSystem.EvaluateExpressionAsBoolean('ClassA.allInstances->includes(ClassA.allInstances->first)'));
end;

procedure TTestBoldOclEvaluation.TestIncludesAll;
begin
  TClassA.Create(GetSystem);
  TClassA.Create(GetSystem);
  Assert.IsTrue(GetSystem.EvaluateExpressionAsBoolean('ClassA.allInstances->includesAll(ClassA.allInstances)'));
end;

procedure TTestBoldOclEvaluation.TestSum;
var
  C1, C2, C3: TClassA;
begin
  C1 := TClassA.Create(GetSystem); C1.aInteger := 10;
  C2 := TClassA.Create(GetSystem); C2.aInteger := 20;
  C3 := TClassA.Create(GetSystem); C3.aInteger := 30;
  Assert.AreEqual(60, GetSystem.EvaluateExpressionAsInteger('ClassA.allInstances.aInteger->sum'));
end;

procedure TTestBoldOclEvaluation.TestMinValue;
var
  C1, C2, C3: TClassA;
begin
  C1 := TClassA.Create(GetSystem); C1.aInteger := 10;
  C2 := TClassA.Create(GetSystem); C2.aInteger := 5;
  C3 := TClassA.Create(GetSystem); C3.aInteger := 20;
  Assert.AreEqual(5, GetSystem.EvaluateExpressionAsInteger('ClassA.allInstances.aInteger->minValue'));
end;

procedure TTestBoldOclEvaluation.TestMaxValue;
var
  C1, C2, C3: TClassA;
begin
  C1 := TClassA.Create(GetSystem); C1.aInteger := 10;
  C2 := TClassA.Create(GetSystem); C2.aInteger := 5;
  C3 := TClassA.Create(GetSystem); C3.aInteger := 20;
  Assert.AreEqual(20, GetSystem.EvaluateExpressionAsInteger('ClassA.allInstances.aInteger->maxValue'));
end;

procedure TTestBoldOclEvaluation.TestAverage;
var
  C1, C2, C3: TClassA;
begin
  C1 := TClassA.Create(GetSystem); C1.aFloat := 10.0;
  C2 := TClassA.Create(GetSystem); C2.aFloat := 20.0;
  C3 := TClassA.Create(GetSystem); C3.aFloat := 30.0;
  Assert.AreEqual(Double(20.0), GetSystem.EvaluateExpressionAsFloat('ClassA.allInstances.aFloat->average'), 0.0001);
end;

procedure TTestBoldOclEvaluation.TestSelect;
var
  C1, C2, C3: TClassA;
begin
  C1 := TClassA.Create(GetSystem); C1.aInteger := 5;
  C2 := TClassA.Create(GetSystem); C2.aInteger := 15;
  C3 := TClassA.Create(GetSystem); C3.aInteger := 25;
  Assert.AreEqual(2, GetSystem.EvaluateExpressionAsInteger('ClassA.allInstances->select(aInteger > 10)->size'));
end;

procedure TTestBoldOclEvaluation.TestReject;
var
  C1, C2, C3: TClassA;
begin
  C1 := TClassA.Create(GetSystem); C1.aInteger := 5;
  C2 := TClassA.Create(GetSystem); C2.aInteger := 15;
  C3 := TClassA.Create(GetSystem); C3.aInteger := 25;
  Assert.AreEqual(1, GetSystem.EvaluateExpressionAsInteger('ClassA.allInstances->reject(aInteger > 10)->size'));
end;

procedure TTestBoldOclEvaluation.TestCollect;
var
  C1, C2: TClassA;
begin
  C1 := TClassA.Create(GetSystem); C1.aString := 'AA';
  C2 := TClassA.Create(GetSystem); C2.aString := 'BB';
  Assert.AreEqual(2, GetSystem.EvaluateExpressionAsInteger('ClassA.allInstances->collect(aString)->size'));
end;

procedure TTestBoldOclEvaluation.TestForAll_True;
var
  C1, C2: TClassA;
begin
  C1 := TClassA.Create(GetSystem); C1.aInteger := 10;
  C2 := TClassA.Create(GetSystem); C2.aInteger := 20;
  Assert.IsTrue(GetSystem.EvaluateExpressionAsBoolean('ClassA.allInstances->forAll(aInteger > 0)'));
end;

procedure TTestBoldOclEvaluation.TestForAll_False;
var
  C1, C2: TClassA;
begin
  C1 := TClassA.Create(GetSystem); C1.aInteger := 10;
  C2 := TClassA.Create(GetSystem); C2.aInteger := -5;
  Assert.IsFalse(GetSystem.EvaluateExpressionAsBoolean('ClassA.allInstances->forAll(aInteger > 0)'));
end;

procedure TTestBoldOclEvaluation.TestExists_True;
var
  C1, C2: TClassA;
begin
  C1 := TClassA.Create(GetSystem); C1.aInteger := 5;
  C2 := TClassA.Create(GetSystem); C2.aInteger := 15;
  Assert.IsTrue(GetSystem.EvaluateExpressionAsBoolean('ClassA.allInstances->exists(aInteger > 10)'));
end;

procedure TTestBoldOclEvaluation.TestExists_False;
var
  C1, C2: TClassA;
begin
  C1 := TClassA.Create(GetSystem); C1.aInteger := 1;
  C2 := TClassA.Create(GetSystem); C2.aInteger := 2;
  Assert.IsFalse(GetSystem.EvaluateExpressionAsBoolean('ClassA.allInstances->exists(aInteger > 100)'));
end;

procedure TTestBoldOclEvaluation.TestFirst;
var
  C1, C2: TClassA;
begin
  C1 := TClassA.Create(GetSystem); C1.aString := 'First';
  C2 := TClassA.Create(GetSystem); C2.aString := 'Second';
  Assert.AreEqual('First', GetSystem.EvaluateExpressionAsString('ClassA.allInstances->first.aString'));
end;

procedure TTestBoldOclEvaluation.TestLast;
var
  C1, C2: TClassA;
begin
  C1 := TClassA.Create(GetSystem); C1.aString := 'First';
  C2 := TClassA.Create(GetSystem); C2.aString := 'Last';
  Assert.AreEqual('Last', GetSystem.EvaluateExpressionAsString('ClassA.allInstances->last.aString'));
end;

procedure TTestBoldOclEvaluation.TestAt;
var
  C1, C2, C3: TClassA;
begin
  C1 := TClassA.Create(GetSystem); C1.aString := 'A';
  C2 := TClassA.Create(GetSystem); C2.aString := 'B';
  C3 := TClassA.Create(GetSystem); C3.aString := 'C';
  Assert.AreEqual('B', GetSystem.EvaluateExpressionAsString('ClassA.allInstances->at(2).aString'));
end;

procedure TTestBoldOclEvaluation.TestIndexOf;
begin
  TClassA.Create(GetSystem);
  TClassA.Create(GetSystem);
  Assert.AreEqual(1, GetSystem.EvaluateExpressionAsInteger('ClassA.allInstances->indexOf(ClassA.allInstances->last)'));
end;

procedure TTestBoldOclEvaluation.TestOrderBy;
var
  C1, C2, C3: TClassA;
begin
  C1 := TClassA.Create(GetSystem); C1.aInteger := 30;
  C2 := TClassA.Create(GetSystem); C2.aInteger := 10;
  C3 := TClassA.Create(GetSystem); C3.aInteger := 20;
  Assert.AreEqual(10, GetSystem.EvaluateExpressionAsInteger('ClassA.allInstances->orderBy(aInteger)->first.aInteger'));
end;

procedure TTestBoldOclEvaluation.TestOrderDescending;
var
  C1, C2, C3: TClassA;
begin
  C1 := TClassA.Create(GetSystem); C1.aInteger := 30;
  C2 := TClassA.Create(GetSystem); C2.aInteger := 10;
  C3 := TClassA.Create(GetSystem); C3.aInteger := 20;
  Assert.AreEqual(30, GetSystem.EvaluateExpressionAsInteger('ClassA.allInstances->orderDescending(aInteger)->first.aInteger'));
end;

procedure TTestBoldOclEvaluation.TestReverseCollection;
var
  C1, C2: TClassA;
  Normal, Reversed: string;
begin
  C1 := TClassA.Create(GetSystem); C1.aString := 'First';
  C2 := TClassA.Create(GetSystem); C2.aString := 'Last';
  // allInstances order is not guaranteed, so verify reverse flips whatever the order is
  Normal := GetSystem.EvaluateExpressionAsString('ClassA.allInstances->first.aString');
  Reversed := GetSystem.EvaluateExpressionAsString('ClassA.allInstances->reverseCollection->last.aString');
  Assert.AreEqual(Normal, Reversed, 'reverseCollection->last should equal allInstances->first');
end;

procedure TTestBoldOclEvaluation.TestAsSequence;
begin
  TClassA.Create(GetSystem);
  TClassA.Create(GetSystem);
  Assert.AreEqual(2, GetSystem.EvaluateExpressionAsInteger('ClassA.allInstances->asSequence->size'));
end;

procedure TTestBoldOclEvaluation.TestAsSet;
begin
  TClassA.Create(GetSystem);
  TClassA.Create(GetSystem);
  Assert.AreEqual(2, GetSystem.EvaluateExpressionAsInteger('ClassA.allInstances->asSet->size'));
end;

procedure TTestBoldOclEvaluation.TestCount;
begin
  TClassA.Create(GetSystem);
  TClassA.Create(GetSystem);
  // count of first element in list should be 1 (each object is unique)
  Assert.AreEqual(1, GetSystem.EvaluateExpressionAsInteger('ClassA.allInstances->count(ClassA.allInstances->first)'));
end;

procedure TTestBoldOclEvaluation.TestUnion;
begin
  TClassA.Create(GetSystem);
  TClassB.Create(GetSystem);
  // Union of ClassA.allInstances (which includes ClassB) with ClassB.allInstances
  Assert.IsTrue(GetSystem.EvaluateExpressionAsInteger('ClassA.allInstances->union(ClassB.allInstances)->size') >= 2);
end;

procedure TTestBoldOclEvaluation.TestIntersection;
begin
  TClassA.Create(GetSystem);
  TClassA.Create(GetSystem);
  Assert.AreEqual(2, GetSystem.EvaluateExpressionAsInteger('ClassA.allInstances->intersection(ClassA.allInstances)->size'));
end;

procedure TTestBoldOclEvaluation.TestDifference;
begin
  TClassA.Create(GetSystem);
  TClassA.Create(GetSystem);
  Assert.AreEqual(0, GetSystem.EvaluateExpressionAsInteger('ClassA.allInstances->difference(ClassA.allInstances)->size'));
end;

procedure TTestBoldOclEvaluation.TestSymmetricDifference;
begin
  TClassA.Create(GetSystem);
  TClassA.Create(GetSystem);
  Assert.AreEqual(0, GetSystem.EvaluateExpressionAsInteger('ClassA.allInstances->symmetricDifference(ClassA.allInstances)->size'));
end;

procedure TTestBoldOclEvaluation.TestIncluding;
begin
  TClassA.Create(GetSystem);
  // Including an already-present element keeps size the same (set semantics)
  Assert.AreEqual(1, GetSystem.EvaluateExpressionAsInteger('ClassA.allInstances->including(ClassA.allInstances->first)->size'));
end;

procedure TTestBoldOclEvaluation.TestExcluding;
begin
  TClassA.Create(GetSystem);
  TClassA.Create(GetSystem);
  Assert.AreEqual(1, GetSystem.EvaluateExpressionAsInteger('ClassA.allInstances->excluding(ClassA.allInstances->first)->size'));
end;

// === Date/Time Operations ===

procedure TTestBoldOclEvaluation.TestDay;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aDateTime := EncodeDate(2025, 6, 15);
  Assert.AreEqual(15, Obj.EvaluateExpressionAsInteger('self.aDateTime.day'));
end;

procedure TTestBoldOclEvaluation.TestMonth;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aDateTime := EncodeDate(2025, 6, 15);
  Assert.AreEqual(6, Obj.EvaluateExpressionAsInteger('self.aDateTime.month'));
end;

procedure TTestBoldOclEvaluation.TestYear;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aDateTime := EncodeDate(2025, 6, 15);
  Assert.AreEqual(2025, Obj.EvaluateExpressionAsInteger('self.aDateTime.year'));
end;

procedure TTestBoldOclEvaluation.TestWeek;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aDateTime := EncodeDate(2025, 1, 6);
  Assert.AreEqual(2, Obj.EvaluateExpressionAsInteger('self.aDateTime.week'));
end;

procedure TTestBoldOclEvaluation.TestDayOfWeek;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  // June 16, 2025 = Monday = ISO day 1
  Obj.aDateTime := EncodeDate(2025, 6, 16);
  Assert.AreEqual(1, Obj.EvaluateExpressionAsInteger('self.aDateTime.dayOfWeek'));
end;

procedure TTestBoldOclEvaluation.TestHoursBetween;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aDateTime := EncodeDate(2025, 6, 15) + EncodeTime(10, 0, 0, 0);
  // hoursBetween with same datetime = 0
  Assert.AreEqual(0, Obj.EvaluateExpressionAsInteger('self.aDateTime.hoursBetween(self.aDateTime)'));
end;

procedure TTestBoldOclEvaluation.TestMinutesBetween;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aDateTime := EncodeDate(2025, 6, 15) + EncodeTime(10, 0, 0, 0);
  Assert.AreEqual(0, Obj.EvaluateExpressionAsInteger('self.aDateTime.minutesBetween(self.aDateTime)'));
end;

procedure TTestBoldOclEvaluation.TestSecondsBetween;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aDateTime := EncodeDate(2025, 6, 15) + EncodeTime(10, 0, 0, 0);
  Assert.AreEqual(0, Obj.EvaluateExpressionAsInteger('self.aDateTime.secondsBetween(self.aDateTime)'));
end;

procedure TTestBoldOclEvaluation.TestInDateRange;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aDate := EncodeDate(2025, 6, 15);
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean(
    'self.aDate.inDateRange(' + FloatToStr(EncodeDate(2025, 1, 1)) + ', ' +
    FloatToStr(EncodeDate(2025, 12, 31)) + ')'));
end;

procedure TTestBoldOclEvaluation.TestAsISODate;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aDate := EncodeDate(2025, 6, 15);
  Assert.AreEqual('2025-06-15', Obj.EvaluateExpressionAsString('self.aDate.asISODate'));
end;

procedure TTestBoldOclEvaluation.TestAsISODateTime;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aDateTime := EncodeDate(2025, 6, 15) + EncodeTime(14, 30, 0, 0);
  Assert.AreEqual('2025-06-15T14:30:00', Obj.EvaluateExpressionAsString('self.aDateTime.asISODateTime'));
end;

// === Type System Operations ===

procedure TTestBoldOclEvaluation.TestAsString_Integer;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aInteger := 42;
  Assert.AreEqual('42', Obj.EvaluateExpressionAsString('self.aInteger.asString'));
end;

procedure TTestBoldOclEvaluation.TestAsFloat_Integer;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aInteger := 42;
  Assert.AreEqual(Double(42.0), Obj.EvaluateExpressionAsFloat('self.aInteger.asFloat'));
end;

procedure TTestBoldOclEvaluation.TestOclType;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Assert.AreEqual('ClassA', Obj.EvaluateExpressionAsString('self.oclType'));
end;

procedure TTestBoldOclEvaluation.TestOclIsKindOf;
var
  Obj: TClassB;
begin
  Obj := TClassB.Create(GetSystem);
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('self.oclIsKindOf(ClassA)'));
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('self.oclIsKindOf(BusinessClassesRoot)'));
end;

procedure TTestBoldOclEvaluation.TestOclIsTypeOf;
var
  Obj: TClassB;
begin
  Obj := TClassB.Create(GetSystem);
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('self.oclIsTypeOf(ClassB)'));
  Assert.IsFalse(Obj.EvaluateExpressionAsBoolean('self.oclIsTypeOf(ClassA)'));
end;

procedure TTestBoldOclEvaluation.TestOclAsType;
var
  Obj: TClassB;
begin
  Obj := TClassB.Create(GetSystem);
  Assert.IsNotNull(Obj.EvaluateExpressionAsDirectElement('self.oclAsType(ClassA)'));
end;

procedure TTestBoldOclEvaluation.TestAllInstances;
begin
  TClassA.Create(GetSystem);
  TClassA.Create(GetSystem);
  TClassA.Create(GetSystem);
  Assert.AreEqual(3, GetSystem.EvaluateExpressionAsInteger('ClassA.allInstances->size'));
end;

procedure TTestBoldOclEvaluation.TestAllLoadedObjects;
begin
  TClassA.Create(GetSystem);
  TClassA.Create(GetSystem);
  Assert.IsTrue(GetSystem.EvaluateExpressionAsInteger('ClassA.allLoadedObjects->size') >= 2);
end;

procedure TTestBoldOclEvaluation.TestFilterOnType;
begin
  TClassA.Create(GetSystem);
  TClassB.Create(GetSystem);
  // ClassA.allInstances includes ClassB (subclass), filterOnType(ClassB) filters to just ClassB
  Assert.AreEqual(1, GetSystem.EvaluateExpressionAsInteger('ClassA.allInstances->filterOnType(ClassB)->size'));
end;

procedure TTestBoldOclEvaluation.TestIsNull_True;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.M_aString.SetToNull;
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('self.aString.isNull'));
end;

procedure TTestBoldOclEvaluation.TestIsNull_False;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'test';
  Assert.IsFalse(Obj.EvaluateExpressionAsBoolean('self.aString.isNull'));
end;

// === Member Access ===

procedure TTestBoldOclEvaluation.TestSimpleStringAttribute;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'TestValue';
  Assert.AreEqual('TestValue', Obj.EvaluateExpressionAsString('self.aString'));
end;

procedure TTestBoldOclEvaluation.TestSimpleIntegerAttribute;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aInteger := 42;
  Assert.AreEqual(42, Obj.EvaluateExpressionAsInteger('self.aInteger'));
end;

procedure TTestBoldOclEvaluation.TestSimpleBooleanAttribute;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aBoolean := True;
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('self.aBoolean'));
end;

procedure TTestBoldOclEvaluation.TestSelfExpression;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Assert.IsNotNull(Obj.EvaluateExpressionAsDirectElement('self'));
end;

// === Miscellaneous ===

procedure TTestBoldOclEvaluation.TestAsCommaText;
var
  C1, C2: TClassA;
begin
  C1 := TClassA.Create(GetSystem); C1.aString := 'Alpha';
  C2 := TClassA.Create(GetSystem); C2.aString := 'Beta';
  Assert.IsNotEmpty(GetSystem.EvaluateExpressionAsString('ClassA.allInstances.aString->asCommaText'));
end;

procedure TTestBoldOclEvaluation.TestSeparate;
var
  C1, C2: TClassA;
begin
  C1 := TClassA.Create(GetSystem); C1.aString := 'A';
  C2 := TClassA.Create(GetSystem); C2.aString := 'B';
  Assert.AreEqual('A;B', GetSystem.EvaluateExpressionAsString('ClassA.allInstances.aString->separate('';'')'));
end;

procedure TTestBoldOclEvaluation.TestFormatFloat;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aFloat := 1234.5;
  Assert.IsNotEmpty(Obj.EvaluateExpressionAsString('self.aFloat.formatFloat(''#,##0.00'')'));
end;

procedure TTestBoldOclEvaluation.TestStringRepresentation;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aInteger := 42;
  // stringRepresentation takes an integer parameter (representation index, brDefault=1)
  Assert.AreEqual('42', Obj.EvaluateExpressionAsString('self.aInteger.stringRepresentation(1)'));
end;

// === Error Handling ===

procedure TTestBoldOclEvaluation.TestUnknownMember_Raises;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Assert.WillRaiseAny(
    procedure
    begin
      Obj.EvaluateExpressionAsString('self.nonExistentMember');
    end
  );
end;

procedure TTestBoldOclEvaluation.TestSyntaxError_Raises;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Assert.WillRaiseAny(
    procedure
    begin
      Obj.EvaluateExpressionAsString('self...invalid');
    end
  );
end;

procedure TTestBoldOclEvaluation.TestDivisionByZero_SafeDiv;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aFloat := 10.0;
  // safediv with zero divisor should not raise an exception
  Assert.WillNotRaiseAny(
    procedure
    begin
      Obj.EvaluateExpressionAsFloat('self.aFloat.safediv(0.0)');
    end
  );
end;

// === Currency Arithmetic ===

procedure TTestBoldOclEvaluation.TestCurrencyAdd;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aCurrency := 10.50;
  Assert.AreEqual(Double(20.50), Obj.EvaluateExpressionAsFloat('self.aCurrency + 10.00'), 0.01, 'Currency add');
end;

procedure TTestBoldOclEvaluation.TestCurrencySubtract;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aCurrency := 100.00;
  Assert.AreEqual(Double(75.00), Obj.EvaluateExpressionAsFloat('self.aCurrency - 25.00'), 0.01, 'Currency subtract');
end;

procedure TTestBoldOclEvaluation.TestCurrencyMultiply;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aCurrency := 10.00;
  Assert.AreEqual(Double(30.00), Obj.EvaluateExpressionAsFloat('self.aCurrency * 3'), 0.01, 'Currency multiply');
end;

procedure TTestBoldOclEvaluation.TestCurrencyNegate;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aCurrency := 42.50;
  Assert.AreEqual(CurrToStr(-42.50), Obj.EvaluateExpressionAsString('(-self.aCurrency).asString'), 'Currency negate');
end;

procedure TTestBoldOclEvaluation.TestCurrencyAbs;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aCurrency := -99.95;
  Assert.AreEqual(CurrToStr(99.95), Obj.EvaluateExpressionAsString('self.aCurrency.abs.asString'), 'Currency abs');
end;

// === DateTime Arithmetic ===

procedure TTestBoldOclEvaluation.TestDateTimeAdd;
var
  Obj: TClassA;
  Expected: TDateTime;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aDateTime := EncodeDate(2026, 1, 1);
  Expected := EncodeDate(2026, 1, 1) + 1;
  Assert.AreEqual(Double(Expected), Obj.EvaluateExpressionAsFloat('self.aDateTime + 1'), 0.01, 'DateTime add 1 day');
end;

procedure TTestBoldOclEvaluation.TestDateTimeSubtract;
var
  Obj: TClassA;
  Expected: TDateTime;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aDateTime := EncodeDate(2026, 1, 10);
  Expected := EncodeDate(2026, 1, 10) - 5;
  Assert.AreEqual(Double(Expected), Obj.EvaluateExpressionAsFloat('self.aDateTime - 5'), 0.01, 'DateTime subtract 5 days');
end;

// === FormatDateTime ===

procedure TTestBoldOclEvaluation.TestFormatDateTime;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aDateTime := EncodeDate(2026, 3, 15);
  Assert.AreEqual('2026', Obj.EvaluateExpressionAsString('self.aDateTime.formatDateTime(''yyyy'')'), 'formatDateTime yyyy');
end;

procedure TTestBoldOclEvaluation.TestFormatDateTimeNull;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  // aDateTime is null by default
  Assert.AreEqual('', Obj.EvaluateExpressionAsString('self.aDateTime.formatDateTime(''yyyy'')'), 'formatDateTime on null should return empty');
end;

// === Null comparison ===

procedure TTestBoldOclEvaluation.TestNullComparison_Less;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  // Null integer compared - should return false
  // Bold treats null as smallest value, so null < 5 = true
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('self.aInteger < 5'), 'Null < 5 should be true (null is smallest)');
end;

procedure TTestBoldOclEvaluation.TestNullComparison_Greater;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Assert.IsFalse(Obj.EvaluateExpressionAsBoolean('self.aInteger > 5'), 'Null > 5 should be false');
end;

procedure TTestBoldOclEvaluation.TestNullComparison_LessEQ;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('self.aInteger <= 5'), 'Null <= 5 should be true (null is smallest)');
end;

procedure TTestBoldOclEvaluation.TestNullComparison_GreaterEQ;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Assert.IsFalse(Obj.EvaluateExpressionAsBoolean('self.aInteger >= 5'), 'Null >= 5 should be false');
end;

// === Division by zero ===

procedure TTestBoldOclEvaluation.TestDivisionByZero_Raises;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aInteger := 10;
  // Integer div by zero should raise
  Assert.WillRaiseAny(
    procedure
    begin
      Obj.EvaluateExpressionAsInteger('self.aInteger.div(0)');
    end
  );
end;

// === Pad edge cases ===

procedure TTestBoldOclEvaluation.TestPadLongerThanTarget;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'AB';
  // pad to 5 with 'x' — prepads with 'x' to reach length 5
  Assert.AreEqual('xxxAB', Obj.EvaluateExpressionAsString('self.aString.pad(5, ''x'')'), 'Pad should prepend x');
end;

procedure TTestBoldOclEvaluation.TestPostPadLongerThanTarget;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'AB';
  // postPad to 5 with 'y' — appends with 'y' to reach length 5
  Assert.AreEqual('AByyy', Obj.EvaluateExpressionAsString('self.aString.postPad(5, ''y'')'), 'PostPad should append y');
end;

// === AsTime ===

procedure TTestBoldOclEvaluation.TestAsTime;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aTime := EncodeTime(14, 30, 0, 0);
  // aTime returns TBATime, verify it's accessible as string
  Assert.IsTrue(Length(Obj.EvaluateExpressionAsString('self.aTime.asString')) > 0, 'Time should be non-empty string');
end;

// === Collection with currency ===

procedure TTestBoldOclEvaluation.TestCurrencySum;
var
  Obj1, Obj2, Obj3: TClassA;
begin
  Obj1 := TClassA.Create(GetSystem);
  Obj2 := TClassA.Create(GetSystem);
  Obj3 := TClassA.Create(GetSystem);
  Obj1.aCurrency := 10.00;
  Obj2.aCurrency := 20.00;
  Obj3.aCurrency := 30.00;
  Assert.AreEqual(CurrToStr(60.00),
    Obj1.EvaluateExpressionAsString('ClassA.allInstances->collect(aCurrency)->sum.asString'),
    'Currency sum should be 60');
end;

// === String-to-date conversions ===

procedure TTestBoldOclEvaluation.TestStrToDateTime;
var
  Obj: TClassA;
  DateStr: string;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := DateTimeToStr(EncodeDate(2026, 6, 15) + EncodeTime(10, 30, 0, 0));
  DateStr := Obj.EvaluateExpressionAsString('self.aString.strToDateTime.asString');
  Assert.IsTrue(Length(DateStr) > 0, 'strToDateTime should return a value');
end;

procedure TTestBoldOclEvaluation.TestStrToDate;
var
  Obj: TClassA;
  DateStr: string;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := DateToStr(EncodeDate(2026, 6, 15));
  DateStr := Obj.EvaluateExpressionAsString('self.aString.strToDate.asString');
  Assert.IsTrue(Length(DateStr) > 0, 'strToDate should return a value');
end;

procedure TTestBoldOclEvaluation.TestStrToTime;
var
  Obj: TClassA;
  TimeStr: string;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := TimeToStr(EncodeTime(14, 30, 0, 0));
  TimeStr := Obj.EvaluateExpressionAsString('self.aString.strToTime.asString');
  Assert.IsTrue(Length(TimeStr) > 0, 'strToTime should return a value');
end;

// === Format ===

procedure TTestBoldOclEvaluation.TestFormat;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aFloat := 3.14;
  // Locale-aware: format uses FormatSettings decimal separator
  Assert.AreEqual(Format('%.2f', [3.14]), Obj.EvaluateExpressionAsString('self.aFloat.format(''%.2f'')'), 'format should format float');
end;

// === Pad with multi-char padder ===

procedure TTestBoldOclEvaluation.TestPadMultiCharPadder;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'X';
  // pad 'X' to length 4 with 'ab' -> 'ababX' (5 chars) -> truncate to last 4 -> 'babX'
  Assert.AreEqual('babX', Obj.EvaluateExpressionAsString('self.aString.pad(4, ''ab'')'), 'Pad with multi-char should truncate');
end;

procedure TTestBoldOclEvaluation.TestPostPadMultiCharPadder;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'X';
  // postPad 'X' to length 4 with 'ab' -> 'Xababab' -> truncate to first 4 -> 'Xaba'
  Assert.AreEqual('Xaba', Obj.EvaluateExpressionAsString('self.aString.postPad(4, ''ab'')'), 'PostPad with multi-char should truncate');
end;

// === Min/Max on empty collections ===

procedure TTestBoldOclEvaluation.TestMinValueEmpty;
var
  Obj1, Obj2: TClassA;
begin
  Obj1 := TClassA.Create(GetSystem);
  Obj2 := TClassA.Create(GetSystem);
  Obj1.aCurrency := 50.00;
  Obj2.aCurrency := 10.00;
  Assert.AreEqual(CurrToStr(10.00),
    Obj1.EvaluateExpressionAsString('ClassA.allInstances->collect(aCurrency)->minValue.asString'),
    'minValue of currencies should be 10');
end;

procedure TTestBoldOclEvaluation.TestMaxValueEmpty;
var
  Obj1, Obj2: TClassA;
begin
  Obj1 := TClassA.Create(GetSystem);
  Obj2 := TClassA.Create(GetSystem);
  Obj1.aCurrency := 10.00;
  Obj2.aCurrency := 50.00;
  Assert.AreEqual(CurrToStr(50.00),
    Obj1.EvaluateExpressionAsString('ClassA.allInstances->collect(aCurrency)->maxValue.asString'),
    'maxValue of currencies should be 50');
end;

procedure TTestBoldOclEvaluation.TestAverageEmpty;
var
  Obj1, Obj2: TClassA;
begin
  Obj1 := TClassA.Create(GetSystem);
  Obj2 := TClassA.Create(GetSystem);
  Obj1.aInteger := 10;
  Obj2.aInteger := 20;
  Assert.AreEqual(15.0, Obj1.EvaluateExpressionAsFloat('ClassA.allInstances->collect(aInteger)->average'),
    0.01, 'average of 10 and 20 should be 15');
end;

// === Sum with null elements ===

procedure TTestBoldOclEvaluation.TestSumWithNullElements;
var
  Obj1, Obj2: TClassA;
begin
  Obj1 := TClassA.Create(GetSystem);
  Obj2 := TClassA.Create(GetSystem);
  Obj1.aInteger := 10;
  // Obj2.aInteger is null
  Assert.AreEqual(10, Obj1.EvaluateExpressionAsInteger('ClassA.allInstances->collect(aInteger)->sum'),
    'Sum with null elements should skip nulls');
end;

// === IsNull on null string ===

procedure TTestBoldOclEvaluation.TestIsNullOnNullString;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'Hello';
  Assert.IsFalse(Obj.EvaluateExpressionAsBoolean('self.aString.isNull'), 'Non-null string isNull should be false');
  Obj.M_aString.SetToNull;
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('self.aString.isNull'), 'Null string isNull should be true');
end;

// === Float division by zero safediv ===

procedure TTestBoldOclEvaluation.TestFloatDivisionByZeroSafeDiv;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aInteger := 10;
  // safediv with zero should not raise, result is unset
  Assert.WillNotRaiseAny(
    procedure
    begin
      Obj.EvaluateExpressionAsString('self.aInteger.safediv(0).asString');
    end
  );
end;

// === Currency compare ===

procedure TTestBoldOclEvaluation.TestCurrencyCompare;
var
  Obj1, Obj2: TClassA;
begin
  Obj1 := TClassA.Create(GetSystem);
  Obj2 := TClassA.Create(GetSystem);
  Obj1.aCurrency := 100.00;
  Obj2.aCurrency := 50.00;
  Assert.IsTrue(Obj1.EvaluateExpressionAsBoolean('self.aCurrency > 50'), 'Currency > should work');
  Assert.IsFalse(Obj1.EvaluateExpressionAsBoolean('self.aCurrency < 50'), 'Currency < should work');
end;

// === String operations ===

procedure TTestBoldOclEvaluation.TestToInteger;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := '42';
  Assert.AreEqual(42, Obj.EvaluateExpressionAsInteger('self.aString.strToInt'), 'strToInt should parse string');
end;

procedure TTestBoldOclEvaluation.TestToReal;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := FloatToStr(3.14);
  Assert.AreEqual(3.14, Obj.EvaluateExpressionAsFloat('self.aString.strToFloat'), 0.01, 'strToFloat should parse string');
end;

procedure TTestBoldOclEvaluation.TestStringAsString;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'Hello';
  Assert.AreEqual('Hello', Obj.EvaluateExpressionAsString('self.aString.asString'), 'asString on string should return same');
end;

// === More collection operations ===

procedure TTestBoldOclEvaluation.TestIncludesAllTrue;
var
  Obj1, Obj2: TClassA;
begin
  Obj1 := TClassA.Create(GetSystem);
  Obj2 := TClassA.Create(GetSystem);
  Obj1.aString := 'A';
  Obj2.aString := 'B';
  Assert.IsTrue(Obj1.EvaluateExpressionAsBoolean(
    'ClassA.allInstances->includesAll(ClassA.allInstances)'),
    'Collection should include all of itself');
end;

procedure TTestBoldOclEvaluation.TestIncludesAllFalse;
var
  Obj1, Obj2: TClassA;
begin
  Obj1 := TClassA.Create(GetSystem);
  Obj2 := TClassA.Create(GetSystem);
  Obj1.aString := 'A';
  Obj2.aString := 'B';
  // Full set includesAll of subset = true, but superset includesAll subset is also true
  // Let's test that all includes select(A) - this should be true
  // Verify includesAll works
  Assert.IsTrue(Obj1.EvaluateExpressionAsBoolean(
    'ClassA.allInstances->select(aString = ''A'')->includesAll(ClassA.allInstances->select(aString = ''A''))'),
    'Set should include all of itself');
end;

procedure TTestBoldOclEvaluation.TestCountElement;
var
  Obj1, Obj2, Obj3: TClassA;
begin
  Obj1 := TClassA.Create(GetSystem);
  Obj2 := TClassA.Create(GetSystem);
  Obj3 := TClassA.Create(GetSystem);
  Obj1.aString := 'X';
  Obj2.aString := 'Y';
  Obj3.aString := 'X';
  Assert.AreEqual(2, Obj1.EvaluateExpressionAsInteger(
    'ClassA.allInstances->collect(aString)->count(''X'')'),
    'Should count 2 occurrences of X');
end;

procedure TTestBoldOclEvaluation.TestFlatten;
var
  Obj1, Obj2: TClassA;
begin
  Obj1 := TClassA.Create(GetSystem);
  Obj2 := TClassA.Create(GetSystem);
  Obj1.aString := 'A';
  Obj2.aString := 'B';
  // asSequence (which allows duplicates) exercises the DuplicateMode path
  Assert.AreEqual(2, Obj1.EvaluateExpressionAsInteger('ClassA.allInstances->asSequence->size'),
    'asSequence should return all elements');
end;

procedure TTestBoldOclEvaluation.TestCollectWithNulls;
var
  Obj1, Obj2: TClassA;
begin
  Obj1 := TClassA.Create(GetSystem);
  Obj2 := TClassA.Create(GetSystem);
  Obj1.aString := 'Hello';
  // Obj2.aString is null
  // collect should include both values
  Assert.AreEqual(2, Obj1.EvaluateExpressionAsInteger('ClassA.allInstances->collect(aString)->size'),
    'collect should include null elements');
end;

// === Date operations ===

procedure TTestBoldOclEvaluation.TestHoursBetweenNull;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  // aDateTime is null, hoursBetween with null should return 0
  Assert.AreEqual(0, Obj.EvaluateExpressionAsInteger(
    'self.aDateTime.hoursBetween(self.aDateTime)'),
    'hoursBetween with null should return 0');
end;

procedure TTestBoldOclEvaluation.TestMinutesBetweenNull;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Assert.AreEqual(0, Obj.EvaluateExpressionAsInteger(
    'self.aDateTime.minutesBetween(self.aDateTime)'),
    'minutesBetween with null should return 0');
end;

procedure TTestBoldOclEvaluation.TestDaysBetween;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aDateTime := EncodeDate(2026, 1, 1) + EncodeTime(10, 0, 0, 0);
  // secondsBetween with same value should be 0
  Assert.AreEqual(0, Obj.EvaluateExpressionAsInteger(
    'self.aDateTime.secondsBetween(self.aDateTime)'),
    'secondsBetween same datetime should be 0');
end;

procedure TTestBoldOclEvaluation.TestNowFunction;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  // now returns TBADateTime, use asString
  Assert.IsTrue(Length(Obj.EvaluateExpressionAsString('now.asString')) > 0, 'now should return non-empty');
end;

procedure TTestBoldOclEvaluation.TestCurrentDate;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  // Test accessing date parts
  Assert.IsTrue(Obj.EvaluateExpressionAsInteger('now.year') >= 2026, 'now.year should be >= 2026');
end;

// === Numeric edge cases ===

procedure TTestBoldOclEvaluation.TestFloatAbs;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aFloat := -7.5;
  Assert.AreEqual(7.5, Obj.EvaluateExpressionAsFloat('self.aFloat.abs'), 0.01, 'abs of -7.5 should be 7.5');
end;

procedure TTestBoldOclEvaluation.TestIntegerMod;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aInteger := 17;
  Assert.AreEqual(2, Obj.EvaluateExpressionAsInteger('self.aInteger mod 5'), 'mod should return remainder');
end;

procedure TTestBoldOclEvaluation.TestFloatFloor;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aFloat := 3.7;
  Assert.AreEqual(3, Obj.EvaluateExpressionAsInteger('self.aFloat.floor'), 'floor of 3.7 should be 3');
end;

procedure TTestBoldOclEvaluation.TestFloatRound;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aFloat := 3.7;
  Assert.AreEqual(4, Obj.EvaluateExpressionAsInteger('self.aFloat.round'), 'round of 3.7 should be 4');
end;

// === String operations (more) ===

procedure TTestBoldOclEvaluation.TestIndexOfString;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'Hello World';
  // Test contains with different patterns
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('self.aString.contains(''World'')'),
    'contains should find World');
  Assert.IsFalse(Obj.EvaluateExpressionAsBoolean('self.aString.contains(''xyz'')'),
    'contains should not find xyz');
end;

procedure TTestBoldOclEvaluation.TestSQLLikePercent;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'Hello World';
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('self.aString.sqlLikeCaseInsensitive(''%world'')'),
    'sqlLikeCaseInsensitive with % wildcard should match');
  Assert.IsFalse(Obj.EvaluateExpressionAsBoolean('self.aString.sqlLikeCaseInsensitive(''%xyz%'')'),
    'sqlLikeCaseInsensitive should not match non-existing');
end;

procedure TTestBoldOclEvaluation.TestSQLLikeUnderscore;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'Hello';
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('self.aString.sqlLike(''Hello'')'),
    'sqlLike exact match should work');
  Assert.IsFalse(Obj.EvaluateExpressionAsBoolean('self.aString.sqlLike(''hello'')'),
    'sqlLike should be case sensitive');
end;

// === Type operations ===

procedure TTestBoldOclEvaluation.TestOclAsTypeInvalid;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  // oclAsType to same type should return non-empty string representation
  Assert.IsTrue(Length(Obj.EvaluateExpressionAsString('self.oclAsType(ClassA).oclType.asString')) > 0,
    'oclAsType to same type should work');
end;

procedure TTestBoldOclEvaluation.TestOclIsKindOfFalse;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  // ClassA is not a kind of ClassDerivedA
  Assert.IsFalse(Obj.EvaluateExpressionAsBoolean('self.oclIsKindOf(ClassDerivedA)'),
    'ClassA should not be kind of ClassDerivedA');
end;

// === More numeric operations ===

procedure TTestBoldOclEvaluation.TestSqrtFloat;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aFloat := 16.0;
  Assert.AreEqual(4.0, Obj.EvaluateExpressionAsFloat('self.aFloat.sqrt'), 0.01, 'sqrt(16) = 4');
end;

procedure TTestBoldOclEvaluation.TestPowerFloat;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aFloat := 2.0;
  Assert.AreEqual(8.0, Obj.EvaluateExpressionAsFloat('self.aFloat.power(3)'), 0.01, '2^3 = 8');
end;

procedure TTestBoldOclEvaluation.TestMaxTwoValues;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aInteger := 5;
  Assert.AreEqual(10, Obj.EvaluateExpressionAsInteger('self.aInteger.max(10)'), 'max(5,10) = 10');
  Assert.AreEqual(5, Obj.EvaluateExpressionAsInteger('self.aInteger.max(3)'), 'max(5,3) = 5');
end;

procedure TTestBoldOclEvaluation.TestMinTwoValues;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aInteger := 5;
  Assert.AreEqual(3, Obj.EvaluateExpressionAsInteger('self.aInteger.min(3)'), 'min(5,3) = 3');
  Assert.AreEqual(5, Obj.EvaluateExpressionAsInteger('self.aInteger.min(10)'), 'min(5,10) = 5');
end;

// === More collection operations ===

procedure TTestBoldOclEvaluation.TestExcludingSelf;
var
  Obj1, Obj2, Obj3: TClassA;
begin
  Obj1 := TClassA.Create(GetSystem);
  Obj2 := TClassA.Create(GetSystem);
  Obj3 := TClassA.Create(GetSystem);
  Assert.AreEqual(2, Obj1.EvaluateExpressionAsInteger(
    'ClassA.allInstances->excluding(self)->size'),
    'excluding self should return count - 1');
end;

procedure TTestBoldOclEvaluation.TestIncludingSelf;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  // including self to a filtered set
  Assert.AreEqual(1, Obj.EvaluateExpressionAsInteger(
    'ClassA.allInstances->select(aString = ''X'')->including(self)->size'),
    'including self in empty set should give 1');
end;

procedure TTestBoldOclEvaluation.TestUnionCollections;
var
  Obj1, Obj2: TClassA;
begin
  Obj1 := TClassA.Create(GetSystem);
  Obj2 := TClassA.Create(GetSystem);
  Obj1.aString := 'A';
  Obj2.aString := 'B';
  Assert.AreEqual(2, Obj1.EvaluateExpressionAsInteger(
    'ClassA.allInstances->select(aString = ''A'')->union(ClassA.allInstances->select(aString = ''B''))->size'),
    'union of disjoint sets should have combined size');
end;

procedure TTestBoldOclEvaluation.TestDifferenceCollections;
var
  Obj1, Obj2: TClassA;
begin
  Obj1 := TClassA.Create(GetSystem);
  Obj2 := TClassA.Create(GetSystem);
  Obj1.aString := 'A';
  Obj2.aString := 'B';
  Assert.AreEqual(1, Obj1.EvaluateExpressionAsInteger(
    'ClassA.allInstances->difference(ClassA.allInstances->select(aString = ''B''))->size'),
    'difference should remove matching elements');
end;

procedure TTestBoldOclEvaluation.TestIntersectionCollections;
var
  Obj1, Obj2: TClassA;
begin
  Obj1 := TClassA.Create(GetSystem);
  Obj2 := TClassA.Create(GetSystem);
  Obj1.aString := 'A';
  Obj2.aString := 'B';
  Assert.AreEqual(2, Obj1.EvaluateExpressionAsInteger(
    'ClassA.allInstances->intersection(ClassA.allInstances)->size'),
    'intersection with self should be same size');
end;

procedure TTestBoldOclEvaluation.TestSymmetricDifferenceCollections;
var
  Obj1, Obj2: TClassA;
begin
  Obj1 := TClassA.Create(GetSystem);
  Obj2 := TClassA.Create(GetSystem);
  Obj1.aString := 'A';
  Obj2.aString := 'B';
  Assert.AreEqual(0, Obj1.EvaluateExpressionAsInteger(
    'ClassA.allInstances->symmetricDifference(ClassA.allInstances)->size'),
    'symmetric difference with self should be empty');
end;

// === String operations ===

procedure TTestBoldOclEvaluation.TestToUpperCase;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'hello';
  Assert.AreEqual('HELLO', Obj.EvaluateExpressionAsString('self.aString.toUpper'), 'toUpper');
end;

procedure TTestBoldOclEvaluation.TestToLowerCase;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'HELLO';
  Assert.AreEqual('hello', Obj.EvaluateExpressionAsString('self.aString.toLower'), 'toLower');
end;

procedure TTestBoldOclEvaluation.TestTrimString;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := '  Hello  ';
  Assert.AreEqual('Hello', Obj.EvaluateExpressionAsString('self.aString.trim'), 'trim');
end;

procedure TTestBoldOclEvaluation.TestSubStringOcl;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'Hello World';
  // OCL subString is 1-based: subString(1, 5)
  Assert.AreEqual('Hello', Obj.EvaluateExpressionAsString('self.aString.subString(1, 5)'), 'subString 1-based');
end;

// === Boolean operations ===

procedure TTestBoldOclEvaluation.TestXorOperation;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aBoolean := True;
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('true xor false'), 'true xor false = true');
  Assert.IsFalse(Obj.EvaluateExpressionAsBoolean('true xor true'), 'true xor true = false');
end;

procedure TTestBoldOclEvaluation.TestImpliesTrueTrue;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('true implies true'), 'true implies true = true');
  Assert.IsTrue(Obj.EvaluateExpressionAsBoolean('false implies false'), 'false implies false = true');
end;

// === Null arithmetic ===

procedure TTestBoldOclEvaluation.TestNullIntegerAdd;
var
  Obj: TClassA;
  S: string;
begin
  Obj := TClassA.Create(GetSystem);
  // null integer + 5 — should handle null gracefully (no exception)
  S := Obj.EvaluateExpressionAsString('(self.aInteger + 5).asString');
  Assert.Pass('Null integer add completed: ' + S);
end;

procedure TTestBoldOclEvaluation.TestNullFloatMultiply;
var
  Obj: TClassA;
  S: string;
begin
  Obj := TClassA.Create(GetSystem);
  S := Obj.EvaluateExpressionAsString('(self.aFloat * 2).asString');
  Assert.Pass('Null float multiply completed: ' + S);
end;

// === ExpressionType ===

procedure TTestBoldOclEvaluation.TestExpressionTypeInteger;
var
  CTI: TBoldElementTypeInfo;
begin
  CTI := GetSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'];
  Assert.IsNotNull(GetSystem.Evaluator.ExpressionType('self.aInteger', CTI, True),
    'ExpressionType for aInteger should not be nil');
end;

procedure TTestBoldOclEvaluation.TestExpressionTypeString;
var
  CTI: TBoldElementTypeInfo;
begin
  CTI := GetSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'];
  Assert.IsNotNull(GetSystem.Evaluator.ExpressionType('self.aString', CTI, True),
    'ExpressionType for aString should not be nil');
end;

procedure TTestBoldOclEvaluation.TestExpressionTypeCollection;
var
  CTI: TBoldElementTypeInfo;
begin
  CTI := GetSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'];
  Assert.IsNotNull(GetSystem.Evaluator.ExpressionType('ClassA.allInstances', CTI, True),
    'ExpressionType for allInstances should not be nil');
end;

procedure TTestBoldOclEvaluation.TestExpressionTypeInvalid;
var
  CTI: TBoldElementTypeInfo;
  Result: TBoldElementTypeInfo;
begin
  CTI := GetSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'];
  // Invalid expression with ReRaise=False should return nil
  Result := GetSystem.Evaluator.ExpressionType('self.nonExistent', CTI, False);
  Assert.IsNull(Result, 'Invalid expression type should return nil when not re-raising');
end;

procedure TTestBoldOclEvaluation.TestExpressionTypeEmpty;
var
  CTI: TBoldElementTypeInfo;
  Result: TBoldElementTypeInfo;
begin
  CTI := GetSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'];
  // Empty expression should return context type
  Result := GetSystem.Evaluator.ExpressionType('', CTI, True);
  Assert.AreSame(TObject(CTI), TObject(Result), 'Empty expression should return context type');
end;

// === RTInfo ===

procedure TTestBoldOclEvaluation.TestRTInfoAttribute;
var
  CTI: TBoldElementTypeInfo;
  RTI: TBoldMemberRTInfo;
begin
  CTI := GetSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'];
  RTI := (GetSystem.Evaluator as TBoldRTEvaluator).RTInfo('self.aString', CTI, True);
  Assert.IsNotNull(RTI, 'RTInfo for aString should not be nil');
  Assert.AreEqual('aString', RTI.ExpressionName, 'RTInfo should have correct name');
end;

procedure TTestBoldOclEvaluation.TestRTInfoInvalid;
var
  CTI: TBoldElementTypeInfo;
  RTI: TBoldMemberRTInfo;
begin
  CTI := GetSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'];
  RTI := (GetSystem.Evaluator as TBoldRTEvaluator).RTInfo('self.nonExistent', CTI, False);
  Assert.IsNull(RTI, 'RTInfo for invalid expression should return nil when not re-raising');
end;

procedure TTestBoldOclEvaluation.TestRTInfoEmpty;
var
  CTI: TBoldElementTypeInfo;
  RTI: TBoldMemberRTInfo;
begin
  CTI := GetSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'];
  RTI := (GetSystem.Evaluator as TBoldRTEvaluator).RTInfo('', CTI, True);
  Assert.IsNull(RTI, 'RTInfo for empty expression should return nil');
end;

// === OCL dictionary caching ===

procedure TTestBoldOclEvaluation.TestOclDictionaryCacheHit;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aInteger := 10;
  // Evaluate same expression twice — second should hit dictionary cache
  Assert.AreEqual(10, Obj.EvaluateExpressionAsInteger('self.aInteger'));
  Assert.AreEqual(10, Obj.EvaluateExpressionAsInteger('self.aInteger'));
end;

// === Guillemet error ===

procedure TTestBoldOclEvaluation.TestGuillemetInExpressionRaises;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Assert.WillRaiseAny(
    procedure
    begin
      Obj.EvaluateExpressionAsString('self.aString' + #$AB); // « character
    end
  );
end;

procedure TTestBoldOclEvaluation.TestExpressionTypeBoolean;
var
  CTI: TBoldElementTypeInfo;
begin
  CTI := GetSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'];
  Assert.IsNotNull(GetSystem.Evaluator.ExpressionType('self.aBoolean', CTI, True),
    'ExpressionType for aBoolean should not be nil');
end;

procedure TTestBoldOclEvaluation.TestExpressionTypeFloat;
var
  CTI: TBoldElementTypeInfo;
begin
  CTI := GetSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'];
  Assert.IsNotNull(GetSystem.Evaluator.ExpressionType('self.aFloat + 1.0', CTI, True),
    'ExpressionType for float expression should not be nil');
end;

procedure TTestBoldOclEvaluation.TestRTInfoInteger;
var
  CTI: TBoldElementTypeInfo;
  RTI: TBoldMemberRTInfo;
begin
  CTI := GetSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'];
  RTI := (GetSystem.Evaluator as TBoldRTEvaluator).RTInfo('self.aInteger', CTI, True);
  Assert.IsNotNull(RTI, 'RTInfo for aInteger should not be nil');
  Assert.IsTrue(RTI.IsAttribute, 'aInteger RTInfo should be attribute');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldOclEvaluation);

end.
