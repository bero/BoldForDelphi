unit jano_multiplicity;

interface

uses
  DUnitX.TestFramework,
  BoldUMLTypes,
  SysUtils;

type
  [TestFixture]
  TBoldTest = class
  private
    procedure AssertRange(Range: TBoldUMLRange; lower, upper: integer; UpperUnlimited: boolean);
    procedure AssertSetAsString(SetString: string; lower, upper: integer; upperUmlimited: boolean; GetString: string='');
  public
    [Test]
    procedure UMLRange;
    [Test]
    procedure UMLMultiplicity;
  end;

implementation

procedure TBoldTest.AssertRange(Range: TBoldUMLRange; lower, upper: integer; UpperUnlimited: boolean);
begin
  Assert.AreEqual(lower, range.lower);
  Assert.AreEqual(UpperUnlimited, range.UpperUnlimited);
  if not UpperUnlimited then
    Assert.AreEqual(upper, range.upper);
end;

procedure TBoldTest.AssertSetAsString(SetString: string; lower,
  upper: integer; upperUmlimited: boolean; GetString: string);
var
  aRange: TBoldUMLRange;
begin
  aRange := TBoldUMLRange.Create;
  if GetString = '' then
    GetString := SetString;
  try
    aRange.AsString := SetString;
    AssertRange(arange, lower, upper, upperUmlimited);
    Assert.AreEqual(GetString, aRange.AsString);
  finally
    arange.Free;
  end;
end;

procedure TBoldTest.UMLRange;
var
  aRange: TBoldUMLRange;
begin
  AssertSetAsString('0',     0, 0,  false);
  AssertSetAsString('1',     1, 1,  false);
  AssertSetAsString('9',     9, 9,  false);
  AssertSetAsString(' 9 ',     9, 9,  false, '9');
  AssertSetAsString('0..1',  0, 1,  false);
  AssertSetAsString(' 0..1 ',  0, 1,  false, '0..1');
  AssertSetAsString('0..n',  0, 17, true, 'n');
  AssertSetAsString('0..*',  0, 17, true, 'n');
  AssertSetAsString('0..-1', 0, 17, true, 'n');
  AssertSetAsString('1..n',  1, 17, true);
  AssertSetAsString('1..*',  1, 17, true, '1..n');
  AssertSetAsString('1..-1', 1, 17, true, '1..n');
  AssertSetAsString('9..n', 9, 17, true);
  AssertSetAsString('9..*',  9, 17, true, '9..n');
  AssertSetAsString('9..-1', 9, 17, true, '9..n');

  aRange := TBoldUMLRange.Create;
  try
    AssertRange(arange, 0, 0, false);  // verify newly created range;
    // test formatting
    aRange.AsString := '1..n';
    Assert.AreEqual('1..*', arange.FormatAsString('*'));

    Assert.WillRaise(
      procedure
      begin
        aRange.AsString := 'n..1';
      end,
      EConvertError);
  finally
    FreeAndNil(aRange);
  end;
end;

procedure TBoldTest.UMLMultiplicity;
var
  aMultiplicity: TBoldUMLMultiplicity;
begin
  aMultiplicity := TBoldUMLMultiplicity.Create;
  try
    Assert.AreEqual(0, aMultiplicity.count);
    aMultiplicity.AsString := '0';
    Assert.AreEqual(1, aMultiplicity.count);
    AssertRange(aMultiplicity.Range[0], 0, 0, false);
    Assert.AreEqual('0', aMultiplicity.asstring);

    aMultiplicity.AsString := '0..*';
    Assert.AreEqual(1, aMultiplicity.count);
    AssertRange(aMultiplicity.Range[0], 0, 17, true);
    Assert.AreEqual('n', aMultiplicity.asstring);

    aMultiplicity.AsString := '0..4,5,7';
    Assert.AreEqual(3, aMultiplicity.count);
    AssertRange(aMultiplicity.Range[0], 0, 4, false);
    AssertRange(aMultiplicity.Range[1], 5, 5, false);
    AssertRange(aMultiplicity.Range[2], 7, 7, false);
    Assert.AreEqual('0..4,5,7', aMultiplicity.asstring);
  finally
    FreeAndNil(aMultiplicity);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TBoldTest);

end.
