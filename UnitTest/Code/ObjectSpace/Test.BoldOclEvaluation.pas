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
    procedure TestSafeDivZero;
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
  end;

implementation

uses
  DateUtils,
  Math,
  BoldOclError;

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

procedure TTestBoldOclEvaluation.TestSafeDivZero;
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
begin
  C1 := TClassA.Create(GetSystem); C1.aString := 'First';
  C2 := TClassA.Create(GetSystem); C2.aString := 'Last';
  Assert.AreEqual('Last', GetSystem.EvaluateExpressionAsString('ClassA.allInstances->reverseCollection->first.aString'));
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

initialization
  TDUnitX.RegisterTestFixture(TTestBoldOclEvaluation);

end.
