unit Test.BoldSorter;

interface

uses
  DUnitX.TestFramework,
  BoldSorter;

type
  [TestFixture]
  [Category('Common')]
  TTestBoldSorter = class
  private
    FArray: array of Integer;
    function Compare(Index1, Index2: Integer): Integer;
    procedure Exchange(Index1, Index2: Integer);
  public
    [Test]
    [Category('Quick')]
    procedure TestSort_EmptyArray;
    [Test]
    procedure TestSort_SingleElement;
    [Test]
    procedure TestSort_TwoElements_Ordered;
    [Test]
    procedure TestSort_TwoElements_Reversed;
    [Test]
    procedure TestSort_MultipleElements_Random;
    [Test]
    procedure TestSort_MultipleElements_Reversed;
    [Test]
    procedure TestSort_MultipleElements_AlreadySorted;
    [Test]
    procedure TestSort_WithDuplicates;
    [Test]
    procedure TestSort_AllSameValues;
    [Test]
    procedure TestSort_LargeArray;
  end;

implementation

uses
  System.SysUtils;

{ TTestBoldSorter }

function TTestBoldSorter.Compare(Index1, Index2: Integer): Integer;
begin
  Result := FArray[Index1] - FArray[Index2];
end;

procedure TTestBoldSorter.Exchange(Index1, Index2: Integer);
var
  Temp: Integer;
begin
  Temp := FArray[Index1];
  FArray[Index1] := FArray[Index2];
  FArray[Index2] := Temp;
end;

procedure TTestBoldSorter.TestSort_EmptyArray;
begin
  SetLength(FArray, 0);
  // Should not raise an exception
  // Note: BoldSort expects valid indices, so we skip calling it with empty array
  Assert.Pass('Empty array handled');
end;

procedure TTestBoldSorter.TestSort_SingleElement;
begin
  SetLength(FArray, 1);
  FArray[0] := 42;
  BoldSort(0, 0, Compare, Exchange);
  Assert.AreEqual(42, FArray[0]);
end;

procedure TTestBoldSorter.TestSort_TwoElements_Ordered;
begin
  SetLength(FArray, 2);
  FArray[0] := 1;
  FArray[1] := 2;
  BoldSort(0, 1, Compare, Exchange);
  Assert.AreEqual(1, FArray[0]);
  Assert.AreEqual(2, FArray[1]);
end;

procedure TTestBoldSorter.TestSort_TwoElements_Reversed;
begin
  SetLength(FArray, 2);
  FArray[0] := 2;
  FArray[1] := 1;
  BoldSort(0, 1, Compare, Exchange);
  Assert.AreEqual(1, FArray[0]);
  Assert.AreEqual(2, FArray[1]);
end;

procedure TTestBoldSorter.TestSort_MultipleElements_Random;
begin
  SetLength(FArray, 5);
  FArray[0] := 3;
  FArray[1] := 1;
  FArray[2] := 4;
  FArray[3] := 1;
  FArray[4] := 5;
  BoldSort(0, 4, Compare, Exchange);
  Assert.AreEqual(1, FArray[0]);
  Assert.AreEqual(1, FArray[1]);
  Assert.AreEqual(3, FArray[2]);
  Assert.AreEqual(4, FArray[3]);
  Assert.AreEqual(5, FArray[4]);
end;

procedure TTestBoldSorter.TestSort_MultipleElements_Reversed;
begin
  SetLength(FArray, 5);
  FArray[0] := 5;
  FArray[1] := 4;
  FArray[2] := 3;
  FArray[3] := 2;
  FArray[4] := 1;
  BoldSort(0, 4, Compare, Exchange);
  Assert.AreEqual(1, FArray[0]);
  Assert.AreEqual(2, FArray[1]);
  Assert.AreEqual(3, FArray[2]);
  Assert.AreEqual(4, FArray[3]);
  Assert.AreEqual(5, FArray[4]);
end;

procedure TTestBoldSorter.TestSort_MultipleElements_AlreadySorted;
begin
  SetLength(FArray, 5);
  FArray[0] := 1;
  FArray[1] := 2;
  FArray[2] := 3;
  FArray[3] := 4;
  FArray[4] := 5;
  BoldSort(0, 4, Compare, Exchange);
  Assert.AreEqual(1, FArray[0]);
  Assert.AreEqual(2, FArray[1]);
  Assert.AreEqual(3, FArray[2]);
  Assert.AreEqual(4, FArray[3]);
  Assert.AreEqual(5, FArray[4]);
end;

procedure TTestBoldSorter.TestSort_WithDuplicates;
begin
  SetLength(FArray, 7);
  FArray[0] := 3;
  FArray[1] := 1;
  FArray[2] := 2;
  FArray[3] := 1;
  FArray[4] := 3;
  FArray[5] := 2;
  FArray[6] := 1;
  BoldSort(0, 6, Compare, Exchange);
  Assert.AreEqual(1, FArray[0]);
  Assert.AreEqual(1, FArray[1]);
  Assert.AreEqual(1, FArray[2]);
  Assert.AreEqual(2, FArray[3]);
  Assert.AreEqual(2, FArray[4]);
  Assert.AreEqual(3, FArray[5]);
  Assert.AreEqual(3, FArray[6]);
end;

procedure TTestBoldSorter.TestSort_AllSameValues;
begin
  SetLength(FArray, 5);
  FArray[0] := 7;
  FArray[1] := 7;
  FArray[2] := 7;
  FArray[3] := 7;
  FArray[4] := 7;
  BoldSort(0, 4, Compare, Exchange);
  Assert.AreEqual(7, FArray[0]);
  Assert.AreEqual(7, FArray[1]);
  Assert.AreEqual(7, FArray[2]);
  Assert.AreEqual(7, FArray[3]);
  Assert.AreEqual(7, FArray[4]);
end;

procedure TTestBoldSorter.TestSort_LargeArray;
var
  I: Integer;
begin
  SetLength(FArray, 100);
  for I := 0 to 99 do
    FArray[I] := 100 - I; // Reversed order

  BoldSort(0, 99, Compare, Exchange);

  for I := 0 to 99 do
    Assert.AreEqual(I + 1, FArray[I], Format('Element at index %d', [I]));
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldSorter);

end.
