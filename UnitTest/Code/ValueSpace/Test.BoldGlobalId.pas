unit Test.BoldGlobalId;

interface

uses
  DUnitX.TestFramework,
  BoldGlobalId,
  BoldDefaultId,
  BoldId,
  BoldStreams;

type
  [TestFixture]
  TTestBoldGlobalId = class
  public
    [Test]
    procedure TestCreateWithInfo;
    [Test]
    procedure TestAsString;
    [Test]
    procedure TestGetHash;
    [Test]
    procedure TestGetHashDifferentStrings;
    [Test]
    procedure TestGetIsEqual_SameId;
    [Test]
    procedure TestGetIsEqual_DifferentId;
    [Test]
    procedure TestGetIsEqual_NilId;
    [Test]
    procedure TestGetIsEqual_DifferentClassType;
    [Test]
    procedure TestGetIsStorable;
    [Test]
    procedure TestGetStreamName;
    [Test]
    procedure TestCloneWithClassId;
    [Test]
    procedure TestClassExpressionName;
    [Test]
    procedure TestTopSortedIndex;
    [Test]
    procedure TestTopSortedIndexExact;
  end;

implementation

uses
  SysUtils;

{ TTestBoldGlobalId }

procedure TTestBoldGlobalId.TestCreateWithInfo;
var
  GlobalId: TBoldGlobalId;
begin
  GlobalId := TBoldGlobalId.CreateWithInfo('TestGlobalId123', 5, True, 'TestClass');
  try
    Assert.IsNotNull(GlobalId);
    Assert.AreEqual('TestGlobalId123', GlobalId.AsString);
    Assert.AreEqual('TestClass', GlobalId.ClassExpressionName);
    Assert.AreEqual(5, GlobalId.TopSortedIndex);
    Assert.IsTrue(GlobalId.TopSortedIndexExact);
  finally
    GlobalId.Free;
  end;
end;

procedure TTestBoldGlobalId.TestAsString;
var
  GlobalId: TBoldGlobalId;
begin
  GlobalId := TBoldGlobalId.CreateWithInfo('MyUniqueIdentifier', 1, False, 'SomeClass');
  try
    Assert.AreEqual('MyUniqueIdentifier', GlobalId.AsString);
  finally
    GlobalId.Free;
  end;
end;

procedure TTestBoldGlobalId.TestGetHash;
var
  GlobalId1, GlobalId2: TBoldGlobalId;
begin
  // Same string should produce same hash
  GlobalId1 := TBoldGlobalId.CreateWithInfo('HashTest', 1, True, 'Class1');
  GlobalId2 := TBoldGlobalId.CreateWithInfo('HashTest', 2, False, 'Class2');
  try
    Assert.AreEqual(GlobalId1.Hash, GlobalId2.Hash, 'Same ID string should have same hash');
  finally
    GlobalId1.Free;
    GlobalId2.Free;
  end;
end;

procedure TTestBoldGlobalId.TestGetHashDifferentStrings;
var
  GlobalId1, GlobalId2: TBoldGlobalId;
begin
  // Different strings should (very likely) produce different hashes
  GlobalId1 := TBoldGlobalId.CreateWithInfo('HashTestA', 1, True, 'Class1');
  GlobalId2 := TBoldGlobalId.CreateWithInfo('HashTestB', 1, True, 'Class1');
  try
    Assert.AreNotEqual(GlobalId1.Hash, GlobalId2.Hash, 'Different ID strings should have different hash');
  finally
    GlobalId1.Free;
    GlobalId2.Free;
  end;
end;

procedure TTestBoldGlobalId.TestGetIsEqual_SameId;
var
  GlobalId1, GlobalId2: TBoldGlobalId;
begin
  GlobalId1 := TBoldGlobalId.CreateWithInfo('SameId', 1, True, 'Class1');
  GlobalId2 := TBoldGlobalId.CreateWithInfo('SameId', 2, False, 'Class2');
  try
    // IsEqual compares only the fId field, not class info
    Assert.IsTrue(GlobalId1.IsEqual[GlobalId2], 'IDs with same string should be equal');
  finally
    GlobalId1.Free;
    GlobalId2.Free;
  end;
end;

procedure TTestBoldGlobalId.TestGetIsEqual_DifferentId;
var
  GlobalId1, GlobalId2: TBoldGlobalId;
begin
  GlobalId1 := TBoldGlobalId.CreateWithInfo('Id1', 1, True, 'Class1');
  GlobalId2 := TBoldGlobalId.CreateWithInfo('Id2', 1, True, 'Class1');
  try
    Assert.IsFalse(GlobalId1.IsEqual[GlobalId2], 'IDs with different strings should not be equal');
  finally
    GlobalId1.Free;
    GlobalId2.Free;
  end;
end;

procedure TTestBoldGlobalId.TestGetIsEqual_NilId;
var
  GlobalId: TBoldGlobalId;
begin
  GlobalId := TBoldGlobalId.CreateWithInfo('TestId', 1, True, 'Class1');
  try
    Assert.IsFalse(GlobalId.IsEqual[nil], 'Comparison with nil should return False');
  finally
    GlobalId.Free;
  end;
end;

procedure TTestBoldGlobalId.TestGetIsEqual_DifferentClassType;
var
  GlobalId: TBoldGlobalId;
  DefaultId: TBoldDefaultId;
begin
  GlobalId := TBoldGlobalId.CreateWithInfo('TestId', 1, True, 'Class1');
  DefaultId := TBoldDefaultId.CreateWithClassID(1, True);
  try
    // Different class types should not be equal even with same internal state
    Assert.IsFalse(GlobalId.IsEqual[DefaultId], 'Different class types should not be equal');
  finally
    GlobalId.Free;
    DefaultId.Free;
  end;
end;

procedure TTestBoldGlobalId.TestGetIsStorable;
var
  GlobalId: TBoldGlobalId;
begin
  GlobalId := TBoldGlobalId.CreateWithInfo('StorableTest', 1, True, 'Class1');
  try
    Assert.IsFalse(GlobalId.IsStorable, 'GlobalId should not be storable');
  finally
    GlobalId.Free;
  end;
end;

procedure TTestBoldGlobalId.TestGetStreamName;
var
  GlobalId: TBoldGlobalId;
  Streamable: IBoldStreamable;
begin
  GlobalId := TBoldGlobalId.CreateWithInfo('StreamTest', 1, True, 'Class1');
  try
    Streamable := GlobalId as IBoldStreamable;
    Assert.AreEqual(BOLDGLOBALIDNAME, Streamable.StreamName, 'StreamName should be BOLDGLOBALIDNAME');
  finally
    GlobalId.Free;
  end;
end;

procedure TTestBoldGlobalId.TestCloneWithClassId;
var
  Original, Cloned: TBoldGlobalId;
begin
  Original := TBoldGlobalId.CreateWithInfo('CloneTestId', 5, True, 'OriginalClass');
  try
    Cloned := Original.CloneWithClassId(10, False) as TBoldGlobalId;
    try
      // Cloned should have same ID string and ClassExpressionName
      Assert.AreEqual(Original.AsString, Cloned.AsString, 'Cloned ID should have same AsString');
      Assert.AreEqual(Original.ClassExpressionName, Cloned.ClassExpressionName, 'Cloned should have same ClassExpressionName');
      // But different class info
      Assert.AreEqual(10, Cloned.TopSortedIndex, 'Cloned should have new TopSortedIndex');
      Assert.IsFalse(Cloned.TopSortedIndexExact, 'Cloned should have new TopSortedIndexExact');
    finally
      Cloned.Free;
    end;
  finally
    Original.Free;
  end;
end;

procedure TTestBoldGlobalId.TestClassExpressionName;
var
  GlobalId: TBoldGlobalId;
begin
  GlobalId := TBoldGlobalId.CreateWithInfo('Test', 1, True, 'MyClassName');
  try
    Assert.AreEqual('MyClassName', GlobalId.ClassExpressionName);
  finally
    GlobalId.Free;
  end;
end;

procedure TTestBoldGlobalId.TestTopSortedIndex;
var
  GlobalId: TBoldGlobalId;
begin
  GlobalId := TBoldGlobalId.CreateWithInfo('Test', 42, True, 'Class');
  try
    Assert.AreEqual(42, GlobalId.TopSortedIndex);
  finally
    GlobalId.Free;
  end;
end;

procedure TTestBoldGlobalId.TestTopSortedIndexExact;
var
  GlobalId1, GlobalId2: TBoldGlobalId;
begin
  GlobalId1 := TBoldGlobalId.CreateWithInfo('Test1', 1, True, 'Class');
  GlobalId2 := TBoldGlobalId.CreateWithInfo('Test2', 1, False, 'Class');
  try
    Assert.IsTrue(GlobalId1.TopSortedIndexExact, 'Should be exact');
    Assert.IsFalse(GlobalId2.TopSortedIndexExact, 'Should not be exact');
  finally
    GlobalId1.Free;
    GlobalId2.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldGlobalId);

end.
