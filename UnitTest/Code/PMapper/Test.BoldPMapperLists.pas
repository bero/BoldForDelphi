unit Test.BoldPMapperLists;

interface

uses
  DUnitX.TestFramework,
  BoldPMapperLists;

type
  [TestFixture]
  [Category('PMapper')]
  TTestBoldPMapperLists = class
  public
    [Test]
    procedure TestMemberDescriptorsByIndex;
    [Test]
    procedure TestSystemDescriptorsByIndex;
    [Test]
    procedure TestObjectDescriptorsByIndex;
    [Test]
    procedure TestMemberDescriptorCanStore;
    [Test]
    procedure TestDescriptorForModelNameWithNonDefaultMapper;
    [Test]
    procedure TestDescriptorForModelNameWithUnknownModelName;
  end;

implementation

uses
  BoldDefs,
  BoldTypeNameDictionary;

{ TTestBoldPMapperLists }

procedure TTestBoldPMapperLists.TestMemberDescriptorsByIndex;
var
  Descriptor: TBoldMemberPersistenceMapperDescriptor;
  i: Integer;
begin
  // Test that Descriptors[index] works
  // Covers lines 241-243
  Assert.IsTrue(BoldMemberPersistenceMappers.Count > 0, 'BoldMemberPersistenceMappers should have registered descriptors');
  for i := 0 to BoldMemberPersistenceMappers.Count - 1 do
  begin
    Descriptor := BoldMemberPersistenceMappers.Descriptors[i];
    Assert.IsNotNull(Descriptor);
    Assert.IsNotNull(Descriptor.MemberPersistenceMapperClass);
  end;
end;

procedure TTestBoldPMapperLists.TestSystemDescriptorsByIndex;
var
  Descriptor: TBoldSystemPersistenceMapperDescriptor;
  i: Integer;
begin
  // Test that Descriptors[index] works
  // Covers lines 291-293
  Assert.IsTrue(BoldSystemPersistenceMappers.Count > 0, 'BoldSystemPersistenceMappers should have registered descriptors');
  for i := 0 to BoldSystemPersistenceMappers.Count - 1 do
  begin
    Descriptor := BoldSystemPersistenceMappers.Descriptors[i];
    Assert.IsNotNull(Descriptor);
    Assert.IsNotNull(Descriptor.SystemPersistenceMapperClass);
  end;
end;

procedure TTestBoldPMapperLists.TestObjectDescriptorsByIndex;
var
  Descriptor: TBoldObjectPersistenceMapperDescriptor;
  i: Integer;
begin
  // Test that Descriptors[index] works
  // Covers lines 326-328
  Assert.IsTrue(BoldObjectPersistenceMappers.Count > 0, 'BoldObjectPersistenceMappers should have registered descriptors');
  for i := 0 to BoldObjectPersistenceMappers.Count - 1 do
  begin
    Descriptor := BoldObjectPersistenceMappers.Descriptors[i];
    Assert.IsNotNull(Descriptor);
    Assert.IsNotNull(Descriptor.ObjectPersistenceMapper);
  end;
end;

procedure TTestBoldPMapperLists.TestMemberDescriptorCanStore;
var
  Descriptor: TBoldMemberPersistenceMapperDescriptor;
begin
  // Test CanStore method on a descriptor
  // Covers lines 273-275
  Assert.IsTrue(BoldMemberPersistenceMappers.Count > 0, 'BoldMemberPersistenceMappers should have registered descriptors');
  Descriptor := BoldMemberPersistenceMappers.Descriptors[0];
  // CanStore returns boolean - just verify it doesn't raise an exception
  Descriptor.CanStore('String');
  Assert.Pass('CanStore executed without exception');
end;

procedure TTestBoldPMapperLists.TestDescriptorForModelNameWithNonDefaultMapper;
var
  Dict: TBoldTypeNameDictionary;
  Descriptor: TBoldMemberPersistenceMapperDescriptor;
begin
  // Test with non-default mapper name
  // Covers line 366: ActualmapperName := DefaultMapperName;
  Dict := TBoldTypeNameDictionary.Create(nil);
  try
    // Pass a specific mapper name instead of 'default'
    Descriptor := BoldMemberPersistenceMappers.DescriptorForModelNameWithDefaultSupport(
      'NonExistent', 'SomeSpecificMapper', Dict);
    // Result will be nil since 'SomeSpecificMapper' doesn't exist
    Assert.IsNull(Descriptor);
  finally
    Dict.Free;
  end;
end;

procedure TTestBoldPMapperLists.TestDescriptorForModelNameWithUnknownModelName;
var
  Dict: TBoldTypeNameDictionary;
  Descriptor: TBoldMemberPersistenceMapperDescriptor;
begin
  // Test with unknown model name and default mapper
  // Covers lines 358 (DEFAULTNAME lookup) and 361 (empty ActualMapperName)
  Dict := TBoldTypeNameDictionary.Create(nil);
  try
    // Empty dictionary means ModelName won't be found, and DEFAULTNAME won't be found either
    Descriptor := BoldMemberPersistenceMappers.DescriptorForModelNameWithDefaultSupport(
      'NonExistentModelName', DEFAULTNAME, Dict);
    // Result will be nil since no mapping found
    Assert.IsNull(Descriptor);
  finally
    Dict.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldPMapperLists);

end.
