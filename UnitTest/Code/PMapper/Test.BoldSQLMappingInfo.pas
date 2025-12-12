unit Test.BoldSQLMappingInfo;

interface

uses
  DUnitX.TestFramework,
  BoldSQLMappingInfo,
  BoldMappingInfo,
  BoldTaggedValueSupport;

type
  // Subclass to expose protected methods for testing
  TTestableMappingInfo = class(TBoldDefaultMappingInfo)
  public
    procedure CallClearMappingInfo;
  end;

  [TestFixture]
  [Category('PMapper')]
  TTestBoldSQLMappingInfo = class
  private
    FMappingInfo: TTestableMappingInfo;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestAddDuplicateAllInstancesMapping;
    [Test]
    procedure TestAddDuplicateMemberMapping;
    [Test]
    procedure TestAddDuplicateObjectStorageMapping;
    [Test]
    procedure TestMemberMappingCompareMapping;
    [Test]
    procedure TestAllInstancesMappingCompareMapping;
    [Test]
    procedure TestObjectStorageMappingCompareMapping;
    [Test]
    procedure TestMemberMappingListFillFromList;
    [Test]
    procedure TestClearMappingInfo;
    [Test]
    procedure TestMemberMappingCompareTypeWithInheritance;
    [Test]
    procedure TestAddTypeIdMappingEmptyClassNameRaisesException;
    [Test]
    procedure TestAddTypeIdMappingDuplicateDifferentTypeRaisesException;
    [Test]
    procedure TestCloneWithoutDbType;
  end;

implementation

uses
  BoldDefs;

{ TTestBoldSQLMappingInfo }

{ TTestableMappingInfo }

procedure TTestableMappingInfo.CallClearMappingInfo;
begin
  ClearMappingInfo;
end;

{ TTestBoldSQLMappingInfo }

procedure TTestBoldSQLMappingInfo.Setup;
begin
  FMappingInfo := TTestableMappingInfo.Create('BOLD', 128, nccDefault);
end;

procedure TTestBoldSQLMappingInfo.TearDown;
begin
  FMappingInfo.Free;
end;

procedure TTestBoldSQLMappingInfo.TestAddDuplicateAllInstancesMapping;
begin
  // Add a mapping
  FMappingInfo.AddAllInstancesMapping('TestClass', 'TestTable', False);
  Assert.AreEqual(1, FMappingInfo.AllInstancesMappings.Count);

  // Add duplicate - should update existing, not add new
  // Covers lines 237, 239-241
  FMappingInfo.AddAllInstancesMapping('TestClass', 'TestTable', True);
  Assert.AreEqual(1, FMappingInfo.AllInstancesMappings.Count);

  // ClassIdRequired should now be True (OR of False and True)
  Assert.IsTrue(FMappingInfo.AllInstancesMappings[0].ClassIdRequired);
end;

procedure TTestBoldSQLMappingInfo.TestAddDuplicateMemberMapping;
begin
  // Add a mapping
  FMappingInfo.AddMemberMapping('TestClass', 'TestMember', 'TestTable', 'Col1', 'TBoldPMString', False);
  Assert.AreEqual(1, FMappingInfo.MemberMappings.Count);

  // Add duplicate with same table and columns - should not add
  // Covers lines 254, 258
  FMappingInfo.AddMemberMapping('TestClass', 'TestMember', 'TestTable', 'Col1', 'TBoldPMString', False);
  Assert.AreEqual(1, FMappingInfo.MemberMappings.Count);

  // Add with different column - should add new
  FMappingInfo.AddMemberMapping('TestClass', 'TestMember', 'TestTable', 'Col2', 'TBoldPMString', False);
  Assert.AreEqual(2, FMappingInfo.MemberMappings.Count);
end;

procedure TTestBoldSQLMappingInfo.TestAddDuplicateObjectStorageMapping;
begin
  // Add a mapping
  FMappingInfo.AddObjectStorageMapping('TestClass', 'TestTable');
  Assert.AreEqual(1, FMappingInfo.ObjectStorageMappings.Count);

  // Add duplicate - should not add new
  // Covers line 270
  FMappingInfo.AddObjectStorageMapping('TestClass', 'TestTable');
  Assert.AreEqual(1, FMappingInfo.ObjectStorageMappings.Count);

  // Add with different table - should add
  FMappingInfo.AddObjectStorageMapping('TestClass', 'TestTable2');
  Assert.AreEqual(2, FMappingInfo.ObjectStorageMappings.Count);
end;

procedure TTestBoldSQLMappingInfo.TestMemberMappingCompareMapping;
var
  Mapping1, Mapping2: TBoldMemberMappingInfo;
begin
  // Create two mappings with same table and columns but different mapper
  // Covers lines 347-374 (CompareMapping and CompareType)
  Mapping1 := TBoldMemberMappingInfo.Create('Class1', 'Member1', 'Table1', 'Col1', 'TBoldPMString', False);
  Mapping2 := TBoldMemberMappingInfo.Create('Class1', 'Member1', 'Table1', 'Col1', 'TBoldPMString', False);
  try
    // Same mapping should compare equal
    Assert.IsTrue(Mapping1.CompareMapping(Mapping2));

    // Different table should not be equal
    Mapping2.Free;
    Mapping2 := TBoldMemberMappingInfo.Create('Class1', 'Member1', 'Table2', 'Col1', 'TBoldPMString', False);
    Assert.IsFalse(Mapping1.CompareMapping(Mapping2));
  finally
    Mapping1.Free;
    Mapping2.Free;
  end;
end;

procedure TTestBoldSQLMappingInfo.TestAllInstancesMappingCompareMapping;
var
  Mapping1, Mapping2: TBoldAllInstancesMappingInfo;
begin
  // Covers lines 417-422
  Mapping1 := TBoldAllInstancesMappingInfo.Create('Class1', 'Table1', True);
  Mapping2 := TBoldAllInstancesMappingInfo.Create('Class1', 'Table1', True);
  try
    Assert.IsTrue(Mapping1.CompareMapping(Mapping2));

    // Different ClassIdRequired
    Mapping2.Free;
    Mapping2 := TBoldAllInstancesMappingInfo.Create('Class1', 'Table1', False);
    Assert.IsFalse(Mapping1.CompareMapping(Mapping2));

    // Different table
    Mapping2.Free;
    Mapping2 := TBoldAllInstancesMappingInfo.Create('Class1', 'Table2', True);
    Assert.IsFalse(Mapping1.CompareMapping(Mapping2));
  finally
    Mapping1.Free;
    Mapping2.Free;
  end;
end;

procedure TTestBoldSQLMappingInfo.TestObjectStorageMappingCompareMapping;
var
  Mapping1, Mapping2: TBoldObjectStorageMappingInfo;
begin
  // Covers lines 436-438
  Mapping1 := TBoldObjectStorageMappingInfo.Create('Class1', 'Table1');
  Mapping2 := TBoldObjectStorageMappingInfo.Create('Class1', 'Table1');
  try
    Assert.IsTrue(Mapping1.CompareMapping(Mapping2));

    // Different table
    Mapping2.Free;
    Mapping2 := TBoldObjectStorageMappingInfo.Create('Class1', 'Table2');
    Assert.IsFalse(Mapping1.CompareMapping(Mapping2));
  finally
    Mapping1.Free;
    Mapping2.Free;
  end;
end;

procedure TTestBoldSQLMappingInfo.TestMemberMappingListFillFromList;
var
  SourceList: TBoldMemberMappingList;
begin
  // Covers lines 487-496
  SourceList := TBoldMemberMappingList.Create;
  try
    // Add some mappings to source
    SourceList.Add(TBoldMemberMappingInfo.Create('Class1', 'Member1', 'Table1', 'Col1', 'TBoldPMString', False));
    SourceList.Add(TBoldMemberMappingInfo.Create('Class2', 'Member2', 'Table2', 'Col2', 'TBoldPMInteger', True));

    // Fill target from source
    FMappingInfo.MemberMappings.FillFromList(SourceList);

    Assert.AreEqual(2, FMappingInfo.MemberMappings.Count);
    Assert.AreEqual('Class1', FMappingInfo.MemberMappings[0].ClassExpressionName);
    Assert.AreEqual('Class2', FMappingInfo.MemberMappings[1].ClassExpressionName);
  finally
    SourceList.Free;
  end;
end;

procedure TTestBoldSQLMappingInfo.TestClearMappingInfo;
begin
  // Add some mappings first
  FMappingInfo.AddMemberMapping('Class1', 'Member1', 'Table1', 'Col1', 'TBoldPMString', False);
  FMappingInfo.AddAllInstancesMapping('Class1', 'Table1', True);
  FMappingInfo.AddObjectStorageMapping('Class1', 'Table1');

  Assert.AreEqual(1, FMappingInfo.MemberMappings.Count);
  Assert.AreEqual(1, FMappingInfo.AllInstancesMappings.Count);
  Assert.AreEqual(1, FMappingInfo.ObjectStorageMappings.Count);

  // Clear all mappings - covers lines 276-280
  FMappingInfo.CallClearMappingInfo;

  Assert.AreEqual(0, FMappingInfo.MemberMappings.Count);
  Assert.AreEqual(0, FMappingInfo.AllInstancesMappings.Count);
  Assert.AreEqual(0, FMappingInfo.ObjectStorageMappings.Count);
end;

procedure TTestBoldSQLMappingInfo.TestMemberMappingCompareTypeWithInheritance;
var
  Mapping1, Mapping2: TBoldMemberMappingInfo;
begin
  // Test CompareType with different non-date/non-string mapper types
  // This will reach line 372 which checks IsCompatibleWith
  // Using TBoldPMInteger vs TBoldPMFloat - different types, not compatible
  Mapping1 := TBoldMemberMappingInfo.Create('Class1', 'Member1', 'Table1', 'Col1', 'TBoldPMInteger', False);
  Mapping2 := TBoldMemberMappingInfo.Create('Class1', 'Member1', 'Table1', 'Col1', 'TBoldPMFloat', False);
  try
    // Different incompatible types should return False
    // Covers line 372
    Assert.IsFalse(Mapping1.CompareType(Mapping2));
  finally
    Mapping1.Free;
    Mapping2.Free;
  end;
end;

procedure TTestBoldSQLMappingInfo.TestAddTypeIdMappingEmptyClassNameRaisesException;
begin
  // Covers line 571 - empty ClassExpressionName raises exception
  Assert.WillRaise(
    procedure
    begin
      FMappingInfo.AddTypeIdMapping('', 1);
    end,
    EBoldInternal);
end;

procedure TTestBoldSQLMappingInfo.TestAddTypeIdMappingDuplicateDifferentTypeRaisesException;
begin
  // First add a type mapping
  FMappingInfo.AddTypeIdMapping('TestClass', 1);

  // Adding same class with different type should raise exception
  // Covers line 574
  Assert.WillRaise(
    procedure
    begin
      FMappingInfo.AddTypeIdMapping('TestClass', 2);
    end,
    EBold);
end;

procedure TTestBoldSQLMappingInfo.TestCloneWithoutDbType;
var
  Clone: TBoldSQLMappingInfo;
begin
  // Add some data to clone
  FMappingInfo.AddMemberMapping('Class1', 'Member1', 'Table1', 'Col1', 'TBoldPMString', False);
  FMappingInfo.AddAllInstancesMapping('Class1', 'Table1', True);
  FMappingInfo.AddObjectStorageMapping('Class1', 'Table1');
  FMappingInfo.AddTypeIdMapping('Class1', 1);

  // Clone without DbType - covers lines 647-663
  Clone := FMappingInfo.CloneWithoutDbType;
  try
    // Verify mappings were cloned
    Assert.AreEqual(1, Clone.MemberMappings.Count);
    Assert.AreEqual(1, Clone.AllInstancesMappings.Count);
    Assert.AreEqual(1, Clone.ObjectStorageMappings.Count);
    // DbType should NOT be cloned
    Assert.AreEqual(0, Clone.DbTypeMappings.Count);
  finally
    Clone.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldSQLMappingInfo);

end.
