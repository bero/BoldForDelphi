unit Test.BoldUMLTypes;

interface

uses
  DUnitX.TestFramework,
  BoldTestCase,
  BoldUMLTypes;

type
  [TestFixture]
  [Category('Common')]
  TTestBoldUMLTypes = class(TBoldTestCase)
  private
    procedure AssertRange(Range: TBoldUMLRange; Lower, Upper: Integer; UpperUnlimited: Boolean);
    procedure AssertSetAsString(const SetString: string; Lower, Upper: Integer;
      UpperUnlimited: Boolean; const GetString: string = '');
  public
    [Test]
    procedure TestUMLRange;
    [Test]
    procedure TestUMLMultiplicity;
  end;

implementation

uses
  System.SysUtils;

{ TTestBoldUMLTypes }

procedure TTestBoldUMLTypes.AssertRange(Range: TBoldUMLRange; Lower, Upper: Integer;
  UpperUnlimited: Boolean);
begin
  Assert.AreEqual(Lower, Range.Lower, 'Lower bound mismatch');
  Assert.AreEqual(UpperUnlimited, Range.UpperUnlimited, 'UpperUnlimited mismatch');
  if not UpperUnlimited then
    Assert.AreEqual(Upper, Range.Upper, 'Upper bound mismatch');
end;

procedure TTestBoldUMLTypes.AssertSetAsString(const SetString: string; Lower, Upper: Integer;
  UpperUnlimited: Boolean; const GetString: string);
var
  ARange: TBoldUMLRange;
  ExpectedGetString: string;
begin
  ARange := TBoldUMLRange.Create;
  try
    if GetString = '' then
      ExpectedGetString := SetString
    else
      ExpectedGetString := GetString;

    ARange.AsString := SetString;
    AssertRange(ARange, Lower, Upper, UpperUnlimited);
    Assert.AreEqual(ExpectedGetString, ARange.AsString, 'AsString mismatch');
  finally
    ARange.Free;
  end;
end;

procedure TTestBoldUMLTypes.TestUMLRange;
var
  ARange: TBoldUMLRange;
begin
  AssertSetAsString('0',     0, 0,  False);
  AssertSetAsString('1',     1, 1,  False);
  AssertSetAsString('9',     9, 9,  False);
  AssertSetAsString(' 9 ',   9, 9,  False, '9');
  AssertSetAsString('0..1',  0, 1,  False);
  AssertSetAsString(' 0..1 ', 0, 1, False, '0..1');
  AssertSetAsString('0..n',  0, 17, True, 'n');
  AssertSetAsString('0..*',  0, 17, True, 'n');
  AssertSetAsString('0..-1', 0, 17, True, 'n');
  AssertSetAsString('1..n',  1, 17, True);
  AssertSetAsString('1..*',  1, 17, True, '1..n');
  AssertSetAsString('1..-1', 1, 17, True, '1..n');
  AssertSetAsString('9..n',  9, 17, True);
  AssertSetAsString('9..*',  9, 17, True, '9..n');
  AssertSetAsString('9..-1', 9, 17, True, '9..n');

  ARange := TBoldUMLRange.Create;
  try
    AssertRange(ARange, 0, 0, False);  // verify newly created range

    // test formatting
    ARange.AsString := '1..n';
    Assert.AreEqual('1..*', ARange.FormatAsString('*'), 'FormatAsString mismatch');

    Assert.WillRaise(
      procedure
      begin
        ARange.AsString := 'n..1';
      end,
      EConvertError,
      'Expected EConvertError for invalid range'
    );
  finally
    ARange.Free;
  end;
end;

procedure TTestBoldUMLTypes.TestUMLMultiplicity;
var
  AMultiplicity: TBoldUMLMultiplicity;
begin
  AMultiplicity := TBoldUMLMultiplicity.Create;
  try
    Assert.AreEqual(0, AMultiplicity.Count, 'Initial count should be 0');

    AMultiplicity.AsString := '0';
    Assert.AreEqual(1, AMultiplicity.Count, 'Count should be 1 after setting "0"');
    AssertRange(AMultiplicity.Range[0], 0, 0, False);
    Assert.AreEqual('0', AMultiplicity.AsString);

    AMultiplicity.AsString := '0..*';
    Assert.AreEqual(1, AMultiplicity.Count, 'Count should be 1 after setting "0..*"');
    AssertRange(AMultiplicity.Range[0], 0, 17, True);
    Assert.AreEqual('n', AMultiplicity.AsString);

    AMultiplicity.AsString := '0..4,5,7';
    Assert.AreEqual(3, AMultiplicity.Count, 'Count should be 3 after setting "0..4,5,7"');
    AssertRange(AMultiplicity.Range[0], 0, 4, False);
    AssertRange(AMultiplicity.Range[1], 5, 5, False);
    AssertRange(AMultiplicity.Range[2], 7, 7, False);
    Assert.AreEqual('0..4,5,7', AMultiplicity.AsString);
  finally
    AMultiplicity.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldUMLTypes);

end.
