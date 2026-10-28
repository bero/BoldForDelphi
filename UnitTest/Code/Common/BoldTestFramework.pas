{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
unit BoldTestFramework;
{ The one unit that differs between the Delphi (DUnitX) and the Free Pascal
  (FPCUnit) test builds. Test units use only this unit for the framework and
  are otherwise the same source for both compilers:

    TTestSomething = class(TBoldTestCase)
    protected
      procedure SetUp; override;        // optional, also TearDown,
                                        // SetUpOnce, TearDownOnce
    published
      procedure TestSomething;          // every published method is a test
    end;
    ...
    initialization
      BoldRegisterTest(TTestSomething);

  Delphi: TBoldTestCase is a plain DUnitX fixture. DUnitX runs every published
  method as a test and calls the attributed entry points below. DUnitX binds
  the attributed method itself, not virtually, so those entry points are not
  virtual and call the virtual hooks. Never declare a hook or helper in a
  published section: it would run as a test.
  FPC: TBoldTestCase is an FPCUnit TTestCase, whose SetUp and TearDown are the
  same hooks. Assert is a class with the DUnitX method names over FPCUnit's
  failure exception, so test code calls Assert.AreEqual on both compilers. }

interface

uses
  {$IFDEF FPC}
  SysUtils,
  Classes,
  fpcunit,
  testregistry
  {$ELSE}
  DUnitX.TestFramework
  {$ENDIF};

type
{$IFDEF FPC}
  ETestPass = class(Exception);

  TTestMethod = procedure of object;

  { TBoldTestCase }
  TBoldTestCase = class(TTestCase)
  protected
    procedure RunTest; override;
  end;

  { Assert: the DUnitX Assert API used by the Bold tests, for FPCUnit. }
  Assert = class
  private
    class procedure FailNotEquals(const Expected, Actual, Message: string);
  public
    class procedure Pass(const Message: string = '');
    class procedure Fail(const Message: string = '');
    class procedure AreEqual(const Expected, Actual: string; const Message: string = ''); overload;
    class procedure AreEqual(Expected, Actual: Integer; const Message: string = ''); overload;
    class procedure AreEqual(Expected, Actual: Int64; const Message: string = ''); overload;
    class procedure AreEqual(Expected, Actual: Cardinal; const Message: string = ''); overload;
    class procedure AreEqual(Expected, Actual: Boolean; const Message: string = ''); overload;
    class procedure AreEqual(Expected, Actual, Tolerance: Extended; const Message: string = ''); overload;
    class procedure AreEqual(Expected, Actual: TClass; const Message: string = ''); overload;
    // DUnitX compares objects through its generic AreEqual<T>; FPC 3.2.2 cannot infer T.
    class procedure AreEqual(Expected, Actual: TObject; const Message: string = ''); overload;
    class procedure AreEqual(Expected, Actual: Pointer; const Message: string = ''); overload;
    class procedure AreNotEqual(const Expected, Actual: string; const Message: string = ''); overload;
    class procedure AreNotEqual(Expected, Actual: Integer; const Message: string = ''); overload;
    class procedure AreSame(Expected, Actual: TObject; const Message: string = ''); overload;
    class procedure AreSame(Expected, Actual: Pointer; const Message: string = ''); overload;
    class procedure AreNotSame(Expected, Actual: TObject; const Message: string = ''); overload;
    class procedure IsTrue(Condition: Boolean; const Message: string = '');
    class procedure IsFalse(Condition: Boolean; const Message: string = '');
    class procedure IsNull(Value: TObject; const Message: string = ''); overload;
    class procedure IsNull(Value: Pointer; const Message: string = ''); overload;
    class procedure IsNull(const Value: IInterface; const Message: string = ''); overload;
    class procedure IsNotNull(Value: TObject; const Message: string = ''); overload;
    class procedure IsNotNull(Value: Pointer; const Message: string = ''); overload;
    class procedure IsNotNull(const Value: IInterface; const Message: string = ''); overload;
    class procedure IsNotEmpty(const Value: string; const Message: string = ''); overload;
    class procedure Contains(const Text, SubText: string; const Message: string = ''); overload;
    class procedure StartsWith(const SubText, Text: string; const Message: string = '');
    class procedure WillRaise(Method: TTestMethod; ExceptionClass: ExceptClass = nil; const Message: string = '');
    class procedure WillRaiseAny(Method: TTestMethod; const Message: string = '');
    class procedure WillNotRaiseAny(Method: TTestMethod; const Message: string = '');
  end;
{$ELSE}
  { TBoldTestCase }
  {$M+}
  TBoldTestCase = class
  protected
    procedure SetUpOnce; virtual;
    procedure TearDownOnce; virtual;
    procedure SetUp; virtual;
    procedure TearDown; virtual;
  public
    [SetupFixture]
    procedure DUnitXSetupFixture;
    [TearDownFixture]
    procedure DUnitXTearDownFixture;
    [Setup]
    procedure DUnitXSetup;
    [TearDown]
    procedure DUnitXTearDown;
  end;
  {$M-}

  Assert = DUnitX.TestFramework.Assert;
  // Re-exported so that [Category(...)] in a test unit that uses only this unit resolves;
  // Delphi silently drops an attribute it cannot resolve (W1074).
  CategoryAttribute = DUnitX.TestFramework.CategoryAttribute;
{$ENDIF}

  TBoldTestCaseClass = class of TBoldTestCase;

procedure BoldRegisterTest(AClass: TBoldTestCaseClass);

// The directory for temporary test files, with a trailing path delimiter.
// Windows has TEMP; Linux and macOS do not, so tests must not read TEMP.
function BoldTestTempDir: string;

implementation

{$IFDEF FPC}

procedure BoldRegisterTest(AClass: TBoldTestCaseClass);
begin
  RegisterTest(AClass);
end;

function BoldTestTempDir: string;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir);
end;

{ TBoldTestCase }

procedure TBoldTestCase.RunTest;
begin
  try
    inherited RunTest;
  except
    on ETestPass do
      ; // Assert.Pass ends the test as passed, as in DUnitX
  end;
end;

{ Assert }

class procedure Assert.FailNotEquals(const Expected, Actual, Message: string);
var
  Text: string;
begin
  Text := Format('Expected %s but got %s', [Expected, Actual]);
  if Message <> '' then
    Text := Text + ' ' + Message;
  raise EAssertionFailedError.Create(Text);
end;

class procedure Assert.Pass(const Message: string);
begin
  raise ETestPass.Create(Message);
end;

class procedure Assert.Fail(const Message: string);
begin
  raise EAssertionFailedError.Create(Message);
end;

class procedure Assert.AreEqual(const Expected, Actual: string; const Message: string);
begin
  if Expected <> Actual then
    FailNotEquals(QuotedStr(Expected), QuotedStr(Actual), Message);
end;

class procedure Assert.AreEqual(Expected, Actual: Integer; const Message: string);
begin
  if Expected <> Actual then
    FailNotEquals(IntToStr(Expected), IntToStr(Actual), Message);
end;

class procedure Assert.AreEqual(Expected, Actual: Int64; const Message: string);
begin
  if Expected <> Actual then
    FailNotEquals(IntToStr(Expected), IntToStr(Actual), Message);
end;

class procedure Assert.AreEqual(Expected, Actual: Cardinal; const Message: string);
begin
  if Expected <> Actual then
    FailNotEquals(IntToStr(Expected), IntToStr(Actual), Message);
end;

class procedure Assert.AreEqual(Expected, Actual: Boolean; const Message: string);
begin
  if Expected <> Actual then
    FailNotEquals(BoolToStr(Expected, True), BoolToStr(Actual, True), Message);
end;

class procedure Assert.AreEqual(Expected, Actual, Tolerance: Extended; const Message: string);
begin
  if Abs(Expected - Actual) > Tolerance then
    FailNotEquals(FloatToStr(Expected), FloatToStr(Actual), Message);
end;

class procedure Assert.AreEqual(Expected, Actual: TClass; const Message: string);

  function NameOf(C: TClass): string;
  begin
    if Assigned(C) then
      Result := C.ClassName
    else
      Result := 'nil';
  end;

begin
  if Expected <> Actual then
    FailNotEquals(NameOf(Expected), NameOf(Actual), Message);
end;

class procedure Assert.AreEqual(Expected, Actual: TObject; const Message: string);
begin
  if Expected <> Actual then
    Fail('Objects are not equal ' + Message);
end;

class procedure Assert.AreEqual(Expected, Actual: Pointer; const Message: string);
begin
  if Expected <> Actual then
    Fail('Pointers are not equal ' + Message);
end;

class procedure Assert.AreNotEqual(const Expected, Actual: string; const Message: string);
begin
  if Expected = Actual then
    Fail('Expected a value other than ' + QuotedStr(Actual) + ' ' + Message);
end;

class procedure Assert.AreNotEqual(Expected, Actual: Integer; const Message: string);
begin
  if Expected = Actual then
    Fail('Expected a value other than ' + IntToStr(Actual) + ' ' + Message);
end;

class procedure Assert.AreSame(Expected, Actual: TObject; const Message: string);
begin
  if Expected <> Actual then
    Fail('Objects are not the same ' + Message);
end;

class procedure Assert.AreSame(Expected, Actual: Pointer; const Message: string);
begin
  if Expected <> Actual then
    Fail('Pointers are not the same ' + Message);
end;

class procedure Assert.AreNotSame(Expected, Actual: TObject; const Message: string);
begin
  if Expected = Actual then
    Fail('Objects are the same ' + Message);
end;

class procedure Assert.IsTrue(Condition: Boolean; const Message: string);
begin
  if not Condition then
    Fail('Condition is False ' + Message);
end;

class procedure Assert.IsFalse(Condition: Boolean; const Message: string);
begin
  if Condition then
    Fail('Condition is True ' + Message);
end;

class procedure Assert.IsNull(Value: TObject; const Message: string);
begin
  if Assigned(Value) then
    Fail('Object is not nil ' + Message);
end;

class procedure Assert.IsNull(Value: Pointer; const Message: string);
begin
  if Assigned(Value) then
    Fail('Pointer is not nil ' + Message);
end;

class procedure Assert.IsNull(const Value: IInterface; const Message: string);
begin
  if Assigned(Value) then
    Fail('Interface is not nil ' + Message);
end;

class procedure Assert.IsNotNull(Value: TObject; const Message: string);
begin
  if not Assigned(Value) then
    Fail('Object is nil ' + Message);
end;

class procedure Assert.IsNotNull(Value: Pointer; const Message: string);
begin
  if not Assigned(Value) then
    Fail('Pointer is nil ' + Message);
end;

class procedure Assert.IsNotNull(const Value: IInterface; const Message: string);
begin
  if not Assigned(Value) then
    Fail('Interface is nil ' + Message);
end;

class procedure Assert.IsNotEmpty(const Value: string; const Message: string);
begin
  if Value = '' then
    Fail('String is empty ' + Message);
end;

class procedure Assert.Contains(const Text, SubText: string; const Message: string);
begin
  if Pos(SubText, Text) = 0 then
    Fail(QuotedStr(Text) + ' does not contain ' + QuotedStr(SubText) + ' ' + Message);
end;

class procedure Assert.StartsWith(const SubText, Text: string; const Message: string);
begin
  if Copy(Text, 1, Length(SubText)) <> SubText then
    Fail(QuotedStr(Text) + ' does not start with ' + QuotedStr(SubText) + ' ' + Message);
end;

class procedure Assert.WillRaise(Method: TTestMethod; ExceptionClass: ExceptClass; const Message: string);
begin
  try
    Method;
  except
    on E: Exception do
    begin
      if Assigned(ExceptionClass) and not (E is ExceptionClass) then
        Fail('Expected ' + ExceptionClass.ClassName + ' but ' + E.ClassName + ' was raised ' + Message);
      Exit;
    end;
  end;
  if Assigned(ExceptionClass) then
    Fail('Expected ' + ExceptionClass.ClassName + ' but nothing was raised ' + Message)
  else
    Fail('Expected an exception but nothing was raised ' + Message);
end;

class procedure Assert.WillRaiseAny(Method: TTestMethod; const Message: string);
begin
  WillRaise(Method, nil, Message);
end;

class procedure Assert.WillNotRaiseAny(Method: TTestMethod; const Message: string);
begin
  try
    Method;
  except
    on E: Exception do
      Fail('Unexpected ' + E.ClassName + ': ' + E.Message + ' ' + Message);
  end;
end;

{$ELSE}

uses
  SysUtils,
  IOUtils;

procedure BoldRegisterTest(AClass: TBoldTestCaseClass);
begin
  TDUnitX.RegisterTestFixture(AClass);
end;

function BoldTestTempDir: string;
begin
  Result := IncludeTrailingPathDelimiter(TPath.GetTempPath);
end;

{ TBoldTestCase }

procedure TBoldTestCase.SetUpOnce;
begin
end;

procedure TBoldTestCase.TearDownOnce;
begin
end;

procedure TBoldTestCase.SetUp;
begin
end;

procedure TBoldTestCase.TearDown;
begin
end;

procedure TBoldTestCase.DUnitXSetupFixture;
begin
  SetUpOnce;
end;

procedure TBoldTestCase.DUnitXTearDownFixture;
begin
  TearDownOnce;
end;

procedure TBoldTestCase.DUnitXSetup;
begin
  SetUp;
end;

procedure TBoldTestCase.DUnitXTearDown;
begin
  TearDown;
end;

{$ENDIF}

end.
