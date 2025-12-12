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
    [Test]
    procedure TestUMLRangeInRange;
    [Test]
    procedure TestUMLRangeSetAsStringUnlimited;
    [Test]
    procedure TestUMLMultiplicityInRange;
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

procedure TTestBoldUMLTypes.TestUMLRangeInRange;
var
  ARange: TBoldUMLRange;
begin
  // Test InRange method - covers lines 86-88
  ARange := TBoldUMLRange.Create;
  try
    // Test bounded range 0..5
    ARange.AsString := '0..5';
    Assert.IsTrue(ARange.InRange(0), '0 should be in range 0..5');
    Assert.IsTrue(ARange.InRange(3), '3 should be in range 0..5');
    Assert.IsTrue(ARange.InRange(5), '5 should be in range 0..5');
    Assert.IsFalse(ARange.InRange(-1), '-1 should not be in range 0..5');
    Assert.IsFalse(ARange.InRange(6), '6 should not be in range 0..5');

    // Test unlimited range 1..n
    ARange.AsString := '1..n';
    Assert.IsTrue(ARange.InRange(1), '1 should be in range 1..n');
    Assert.IsTrue(ARange.InRange(1000), '1000 should be in range 1..n');
    Assert.IsFalse(ARange.InRange(0), '0 should not be in range 1..n');
  finally
    ARange.Free;
  end;
end;

procedure TTestBoldUMLTypes.TestUMLRangeSetAsStringUnlimited;
var
  ARange: TBoldUMLRange;
begin
  // Test SetAsString with just 'n' or '*' - covers lines 102-103
  ARange := TBoldUMLRange.Create;
  try
    ARange.AsString := 'n';
    Assert.AreEqual(0, ARange.Lower, 'Lower should be 0 for "n"');
    Assert.IsTrue(ARange.UpperUnlimited, 'UpperUnlimited should be true for "n"');
    Assert.AreEqual('n', ARange.AsString);

    ARange.AsString := '*';
    Assert.AreEqual(0, ARange.Lower, 'Lower should be 0 for "*"');
    Assert.IsTrue(ARange.UpperUnlimited, 'UpperUnlimited should be true for "*"');
    Assert.AreEqual('n', ARange.AsString);
  finally
    ARange.Free;
  end;
end;

procedure TTestBoldUMLTypes.TestUMLMultiplicityInRange;
var
  AMultiplicity: TBoldUMLMultiplicity;
begin
  // Test InRange method on multiplicity - covers lines 174-178
  AMultiplicity := TBoldUMLMultiplicity.Create;
  try
    // Test single range 0..5
    AMultiplicity.AsString := '0..5';
    Assert.IsTrue(AMultiplicity.InRange(0), '0 should be in multiplicity 0..5');
    Assert.IsTrue(AMultiplicity.InRange(3), '3 should be in multiplicity 0..5');
    Assert.IsTrue(AMultiplicity.InRange(5), '5 should be in multiplicity 0..5');
    Assert.IsFalse(AMultiplicity.InRange(6), '6 should not be in multiplicity 0..5');

    // Test disjoint ranges 0..2,5..7
    AMultiplicity.AsString := '0..2,5..7';
    Assert.IsTrue(AMultiplicity.InRange(1), '1 should be in multiplicity 0..2,5..7');
    Assert.IsTrue(AMultiplicity.InRange(6), '6 should be in multiplicity 0..2,5..7');
    Assert.IsFalse(AMultiplicity.InRange(3), '3 should not be in multiplicity 0..2,5..7');
    Assert.IsFalse(AMultiplicity.InRange(4), '4 should not be in multiplicity 0..2,5..7');
  finally
    AMultiplicity.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldUMLTypes);

end.
