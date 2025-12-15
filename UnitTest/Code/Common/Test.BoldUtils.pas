unit Test.BoldUtils;

interface

uses
  DUnitX.TestFramework,
  Classes,
  BoldUtils;

type
  [TestFixture]
  [Category('Common')]
  TTestBoldUtils = class
  public
    [Test]
    [Category('Quick')]
    procedure TestBoldCaseIndependentPosSubstrLongerThanS;
    [Test]
    [Category('Quick')]
    procedure TestBoldSeparateStringListEmpty;
    [Test]
    [Category('Quick')]
    procedure TestBoldSeparateStringListWithAIndex;
    [Test]
    [Category('Quick')]
    procedure TestGetUpperLimitForMultiplicitySimple;
    [Test]
    [Category('Quick')]
    procedure TestGetUpperLimitForMultiplicityNegative;
    [Test]
    [Category('Quick')]
    procedure TestGetUpperLimitForMultiplicityEmpty;
    [Test]
    [Category('Quick')]
    procedure TestBoldAppendToStringsWithCRLF;
    [Test]
    [Category('Quick')]
    procedure TestBoldAppendToStringsLineSplit;
  end;

implementation

uses
  SysUtils,
  BoldDefs;

{ TTestBoldUtils }

procedure TTestBoldUtils.TestBoldCaseIndependentPosSubstrLongerThanS;
begin
  // Covers line 254 - when substr is longer than S
  Assert.AreEqual(0, BoldCaseIndependentPos('LongSubstring', 'Short'));
end;

procedure TTestBoldUtils.TestBoldSeparateStringListEmpty;
var
  SL: TStringList;
begin
  // Covers line 323 - empty list case
  SL := TStringList.Create;
  try
    Assert.AreEqual('', BoldSeparateStringList(SL, ',', '[', ']'));
  finally
    SL.Free;
  end;
end;

procedure TTestBoldUtils.TestBoldSeparateStringListWithAIndex;
var
  SL: TStringList;
begin
  // Covers lines 338, 344 - AIndex parameter
  SL := TStringList.Create;
  try
    SL.Add('Item1');
    SL.Add('Item2');
    SL.Add('Item3');
    // With AIndex = 5, each item should have "5" appended
    Assert.AreEqual('[Item15,Item25,Item35]', BoldSeparateStringList(SL, ',', '[', ']', 5));
  finally
    SL.Free;
  end;
end;

procedure TTestBoldUtils.TestGetUpperLimitForMultiplicitySimple;
begin
  // Covers line 552 - simple multiplicity without ".."
  Assert.AreEqual(5, GetUpperLimitForMultiplicity('5'));
  Assert.AreEqual(1, GetUpperLimitForMultiplicity('1'));
end;

procedure TTestBoldUtils.TestGetUpperLimitForMultiplicityNegative;
begin
  // Covers line 557 - negative result becomes MaxInt
  Assert.AreEqual(MaxInt, GetUpperLimitForMultiplicity('0..-1'));
  Assert.AreEqual(MaxInt, GetUpperLimitForMultiplicity('1..n')); // 'n' parses as MaxInt
  Assert.AreEqual(MaxInt, GetUpperLimitForMultiplicity('*'));
end;

procedure TTestBoldUtils.TestGetUpperLimitForMultiplicityEmpty;
begin
  // Covers line 547 - empty multiplicity returns 1
  Assert.AreEqual(1, GetUpperLimitForMultiplicity(''));
  Assert.AreEqual(1, GetUpperLimitForMultiplicity('   ')); // whitespace only
end;

procedure TTestBoldUtils.TestBoldAppendToStringsWithCRLF;
var
  Strings: TStringList;
begin
  // Covers lines 388-392 - CR/LF replacement with space
  Strings := TStringList.Create;
  try
    BoldAppendToStrings(Strings, 'Hello'#13#10'World', True);
    // CR and LF should be replaced with spaces
    Assert.AreEqual(1, Strings.Count);
    Assert.AreEqual('Hello  World', Strings[0]);
  finally
    Strings.Free;
  end;
end;

procedure TTestBoldUtils.TestBoldAppendToStringsLineSplit;
var
  Strings: TStringList;
  LongStr: string;
begin
  // Covers lines 411-419 - line splitting at 80 chars
  Strings := TStringList.Create;
  try
    // Create a string longer than 80 chars with a space around position 80
    LongStr := 'This is a very long string that should be split into multiple lines because it exceeds the 80 character limit';
    BoldAppendToStrings(Strings, LongStr, True);
    // Should have been split
    Assert.IsTrue(Strings.Count >= 2, 'Long string should be split into multiple lines');
    Assert.IsTrue(Length(Strings[0]) <= 80, 'First line should be <= 80 chars');
  finally
    Strings.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldUtils);

end.
