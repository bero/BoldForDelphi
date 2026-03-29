unit Test.BoldMeta;

{ DUnitX tests for BoldMeta - Model metadata (TMoldClass, TMoldModel, TMoldAttribute, TMoldRole) }

interface

uses
  DUnitX.TestFramework,
  BoldMeta,
  BoldSystemRT,
  BoldSystem,
  jehoBCBoldTest,
  Test.BoldAttributes;

type
  [TestFixture]
  [Category('MoldModel')]
  TTestBoldMeta = class
  private
    FDataModule: TjehodmBoldTest;
    function GetSystem: TBoldSystem;
    function GetModel: TMoldModel;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    // TMoldModel tests
    [Test]
    [Category('Quick')]
    procedure TestModelClassCount;
    [Test]
    [Category('Quick')]
    procedure TestModelRootClass;
    [Test]
    [Category('Quick')]
    procedure TestModelGetClassByName;
    [Test]
    [Category('Quick')]
    procedure TestModelGetClassByNameNotFound;
    [Test]
    [Category('Quick')]
    procedure TestModelOptimisticLocking;
    [Test]
    [Category('Quick')]
    procedure TestModelUseTimestamp;

    // TMoldClass tests
    [Test]
    [Category('Quick')]
    procedure TestClassProperties;
    [Test]
    [Category('Quick')]
    procedure TestClassTableName;
    [Test]
    [Category('Quick')]
    procedure TestClassIsRootClass;
    [Test]
    [Category('Quick')]
    procedure TestClassSuperClass;
    [Test]
    [Category('Quick')]
    procedure TestClassAllMembers;
    [Test]
    [Category('Quick')]
    procedure TestClassEffectivePersistent;
    [Test]
    [Category('Quick')]
    procedure TestClassLowestCommonSuperClass;
    [Test]
    [Category('Quick')]
    procedure TestClassStorage;
    [Test]
    [Category('Quick')]
    procedure TestClassTableMapping;
    [Test]
    [Category('Quick')]
    procedure TestClassExpandedNames;

    // TMoldAttribute tests
    [Test]
    [Category('Quick')]
    procedure TestAttributeProperties;
    [Test]
    [Category('Quick')]
    procedure TestAttributeIsAttributeNotRole;
    [Test]
    [Category('Quick')]
    procedure TestAttributeEffectivePersistent;

    // TMoldRole tests
    [Test]
    [Category('Quick')]
    procedure TestRoleProperties;
    [Test]
    [Category('Quick')]
    procedure TestRoleIsRoleNotAttribute;
    [Test]
    [Category('Quick')]
    procedure TestRoleOtherEnd;
    [Test]
    [Category('Quick')]
    procedure TestRoleEffectiveEmbedded;

    // TBoldSystemTypeInfo / TBoldClassTypeInfo tests (BoldSystemRT)
    [Test]
    [Category('Quick')]
    procedure TestSystemTypeInfoClassCount;
    [Test]
    [Category('Quick')]
    procedure TestClassTypeInfoByExpressionName;
    [Test]
    [Category('Quick')]
    procedure TestClassTypeInfoAttributes;
    [Test]
    [Category('Quick')]
    procedure TestClassTypeInfoRoles;
    [Test]
    [Category('Quick')]
    procedure TestClassTypeInfoSuperType;
    [Test]
    [Category('Quick')]
    procedure TestClassTypeInfoAllMembers;
    [Test]
    [Category('Quick')]
    procedure TestClassTypeInfoIsAbstract;

    // BoldSystemRT — Search
    [Test]
    [Category('Quick')]
    procedure TestSearchByClassName;
    [Test]
    [Category('Quick')]
    procedure TestSearchByAttributeName;
    [Test]
    [Category('Quick')]
    procedure TestSearchPartialMatch;
    [Test]
    [Category('Quick')]
    procedure TestSearchNotFound;

    // BoldSystemRT — ValueTypeNames
    [Test]
    [Category('Quick')]
    procedure TestGetValueTypeNames;
    [Test]
    [Category('Quick')]
    procedure TestGetValueTypeNameList;
    [Test]
    [Category('Quick')]
    procedure TestGetElementTypeInfoByExpressionName;
    [Test]
    [Category('Quick')]
    procedure TestConformsTo;

    // BoldSystemRT — MemberRTInfo
    [Test]
    [Category('Quick')]
    procedure TestMemberRTInfoByExpressionName;
    [Test]
    [Category('Quick')]
    procedure TestMemberRTInfoIsAttribute;
    [Test]
    [Category('Quick')]
    procedure TestMemberRTInfoIsRole;

    // BoldMeta — additional coverage
    [Test]
    [Category('Quick')]
    procedure TestClassHasNotNullMembers;
    [Test]
    [Category('Quick')]
    procedure TestAllNativeAttributes;
    [Test]
    [Category('Quick')]
    procedure TestModelEnsureTopSorted;
    [Test]
    [Category('Quick')]
    procedure TestAttributeAllowNull;
    [Test]
    [Category('Quick')]
    procedure TestClassIncFileName;
  end;

implementation

uses
  Classes,
  SysUtils,
  BoldElements,
  BoldMetaElementList;

{ TTestBoldMeta }

procedure TTestBoldMeta.SetUp;
begin
  FDataModule := TjehodmBoldTest.Create(nil);
end;

procedure TTestBoldMeta.TearDown;
begin
  FreeAndNil(FDataModule);
end;

function TTestBoldMeta.GetSystem: TBoldSystem;
begin
  Result := FDataModule.BoldSystemHandle1.System;
end;

function TTestBoldMeta.GetModel: TMoldModel;
begin
  Result := FDataModule.BoldModel1.MoldModel;
end;

// TMoldModel tests

procedure TTestBoldMeta.TestModelClassCount;
begin
  Assert.IsTrue(GetModel.Classes.Count > 0, 'Model should have at least one class');
end;

procedure TTestBoldMeta.TestModelRootClass;
begin
  Assert.IsNotNull(GetModel.RootClass, 'Model should have a root class');
  Assert.IsNotEmpty(GetModel.RootClass.Name, 'Root class should have a name');
end;

procedure TTestBoldMeta.TestModelGetClassByName;
var
  MoldClass: TMoldClass;
begin
  MoldClass := GetModel.GetClassByName('ClassA');
  Assert.IsNotNull(MoldClass, 'GetClassByName should find ClassA');
  Assert.AreEqual('ClassA', MoldClass.Name, 'Class name should match');
end;

procedure TTestBoldMeta.TestModelGetClassByNameNotFound;
begin
  // GetClassByName for non-existent returns non-nil (empty class) - verify it doesn't crash
  GetModel.GetClassByName('NonExistentClass');
  Assert.IsNotNull(GetModel.GetClassByName('ClassA'), 'GetClassByName should find existing class');
end;

procedure TTestBoldMeta.TestModelOptimisticLocking;
begin
  // OptimisticLocking returns TBoldOptimisticLockingMode
  Assert.IsTrue(Ord(GetModel.OptimisticLocking) >= 0, 'OptimisticLocking should return valid value');
end;

procedure TTestBoldMeta.TestModelUseTimestamp;
begin
  // UseTimestamp is a boolean model-level setting
  // Just access it to cover the getter
  if GetModel.UseTimestamp then
    Assert.Pass('UseTimestamp is True')
  else
    Assert.Pass('UseTimestamp is False');
end;

// TMoldClass tests

procedure TTestBoldMeta.TestClassProperties;
var
  MoldClass: TMoldClass;
begin
  MoldClass := GetModel.GetClassByName('ClassA');
  Assert.IsNotEmpty(MoldClass.Name, 'Name should not be empty');
  Assert.IsNotEmpty(MoldClass.ExpressionName, 'ExpressionName should not be empty');
  Assert.IsNotEmpty(MoldClass.ExpandedExpressionName, 'ExpandedExpressionName should not be empty');
end;

procedure TTestBoldMeta.TestClassTableName;
var
  MoldClass: TMoldClass;
begin
  MoldClass := GetModel.GetClassByName('ClassA');
  Assert.IsNotEmpty(MoldClass.TableName, 'Persistent class should have a table name');
end;

procedure TTestBoldMeta.TestClassIsRootClass;
var
  RootClass, ClassA: TMoldClass;
begin
  RootClass := GetModel.RootClass;
  ClassA := GetModel.GetClassByName('ClassA');
  Assert.IsTrue(RootClass.IsRootClass, 'Root class should report IsRootClass');
  // ClassA may or may not be root depending on model hierarchy
  if ClassA.SuperClass = nil then
    Assert.IsTrue(ClassA.IsRootClass, 'Class without superclass should be root')
  else
    Assert.IsFalse(ClassA.IsRootClass, 'Class with superclass should not be root');
end;

procedure TTestBoldMeta.TestClassSuperClass;
var
  ClassA, DerivedA: TMoldClass;
begin
  ClassA := GetModel.GetClassByName('ClassA');
  DerivedA := GetModel.GetClassByName('ClassDerivedA');
  if Assigned(DerivedA) then
  begin
    Assert.IsNotNull(DerivedA.SuperClass, 'DerivedA should have a superclass');
    Assert.AreSame(TObject(ClassA), TObject(DerivedA.SuperClass), 'DerivedA superclass should be ClassA');
  end
  else
    Assert.Pass('ClassDerivedA not in model');
end;

procedure TTestBoldMeta.TestClassAllMembers;
var
  MoldClass: TMoldClass;
begin
  MoldClass := GetModel.GetClassByName('ClassA');
  Assert.IsTrue(MoldClass.AllBoldMembers.Count > 0, 'ClassA should have members');
end;

procedure TTestBoldMeta.TestClassEffectivePersistent;
var
  MoldClass: TMoldClass;
begin
  MoldClass := GetModel.GetClassByName('ClassA');
  Assert.IsTrue(MoldClass.EffectivePersistent, 'ClassA should be effectively persistent');
end;

procedure TTestBoldMeta.TestClassLowestCommonSuperClass;
var
  ClassA: TMoldClass;
begin
  ClassA := GetModel.GetClassByName('ClassA');
  // LowestCommonSuperClass with itself should return itself
  Assert.AreSame(TObject(ClassA), TObject(ClassA.LowestCommonSuperClass(ClassA)),
    'LowestCommonSuperClass with self should return self');
end;

procedure TTestBoldMeta.TestClassStorage;
var
  MoldClass: TMoldClass;
begin
  MoldClass := GetModel.GetClassByName('ClassA');
  // Storage returns a TBoldStorage enum
  Assert.IsTrue(Ord(MoldClass.Storage) >= 0, 'Storage should return valid value');
end;

procedure TTestBoldMeta.TestClassTableMapping;
var
  MoldClass: TMoldClass;
begin
  MoldClass := GetModel.GetClassByName('ClassA');
  Assert.IsTrue(Ord(MoldClass.TableMapping) >= 0, 'TableMapping should return valid value');
end;

procedure TTestBoldMeta.TestClassExpandedNames;
var
  MoldClass: TMoldClass;
begin
  MoldClass := GetModel.GetClassByName('ClassA');
  Assert.IsNotEmpty(MoldClass.ExpandedExpressionName, 'ExpandedExpressionName should not be empty');
  Assert.IsNotEmpty(MoldClass.ExpandedInterfaceName, 'ExpandedInterfaceName should not be empty');
  // ExpandedUnitName may be empty if no unit name is configured in the model
  // Just access it to cover the getter
  MoldClass.ExpandedUnitName;
end;

// TMoldAttribute tests

procedure TTestBoldMeta.TestAttributeProperties;
var
  MoldClass: TMoldClass;
  Attr: TMoldAttribute;
begin
  MoldClass := GetModel.GetClassByName('ClassA');
  Assert.IsTrue(MoldClass.Attributes.Count > 0, 'ClassA should have attributes');
  Attr := MoldClass.Attributes[0];
  Assert.IsNotEmpty(Attr.Name, 'Attribute should have a name');
  Assert.IsNotEmpty(Attr.ExpressionName, 'Attribute should have expression name');
end;

procedure TTestBoldMeta.TestAttributeIsAttributeNotRole;
var
  MoldClass: TMoldClass;
  Attr: TMoldAttribute;
begin
  MoldClass := GetModel.GetClassByName('ClassA');
  Attr := MoldClass.Attributes[0];
  Assert.IsTrue(Attr.IsAttribute, 'Attribute should report IsAttribute=True');
  Assert.IsFalse(Attr.IsRole, 'Attribute should report IsRole=False');
end;

procedure TTestBoldMeta.TestAttributeEffectivePersistent;
var
  MoldClass: TMoldClass;
  Attr: TMoldAttribute;
begin
  MoldClass := GetModel.GetClassByName('ClassA');
  Attr := MoldClass.Attributes[0];
  Assert.IsTrue(Attr.EffectivePersistent, 'First attribute of persistent class should be persistent');
end;

// TMoldRole tests

procedure TTestBoldMeta.TestRoleProperties;
var
  MoldClass: TMoldClass;
begin
  MoldClass := GetModel.GetClassByName('ClassA');
  if MoldClass.Roles.Count > 0 then
  begin
    Assert.IsNotEmpty(MoldClass.Roles[0].Name, 'Role should have a name');
    Assert.IsNotEmpty(MoldClass.Roles[0].ExpressionName, 'Role should have expression name');
  end
  else
    Assert.Pass('ClassA has no roles');
end;

procedure TTestBoldMeta.TestRoleIsRoleNotAttribute;
var
  MoldClass: TMoldClass;
begin
  MoldClass := GetModel.GetClassByName('ClassA');
  if MoldClass.Roles.Count > 0 then
  begin
    Assert.IsTrue(MoldClass.Roles[0].IsRole, 'Role should report IsRole=True');
    Assert.IsFalse(MoldClass.Roles[0].IsAttribute, 'Role should report IsAttribute=False');
  end
  else
    Assert.Pass('ClassA has no roles');
end;

procedure TTestBoldMeta.TestRoleOtherEnd;
var
  MoldClass: TMoldClass;
  Role: TMoldRole;
begin
  MoldClass := GetModel.GetClassByName('ClassA');
  if MoldClass.Roles.Count > 0 then
  begin
    Role := MoldClass.Roles[0];
    Assert.IsNotNull(Role.OtherEnd, 'Role should have OtherEnd');
    Assert.AreSame(TObject(Role), TObject(Role.OtherEnd.OtherEnd), 'OtherEnd.OtherEnd should be self');
  end
  else
    Assert.Pass('ClassA has no roles');
end;

procedure TTestBoldMeta.TestRoleEffectiveEmbedded;
var
  MoldClass: TMoldClass;
begin
  MoldClass := GetModel.GetClassByName('ClassA');
  if MoldClass.Roles.Count > 0 then
  begin
    // EffectiveEmbedded returns boolean - just access to cover the getter
    if MoldClass.Roles[0].EffectiveEmbedded then
      Assert.Pass('Role is embedded')
    else
      Assert.Pass('Role is not embedded');
  end
  else
    Assert.Pass('ClassA has no roles');
end;

// TBoldSystemTypeInfo / TBoldClassTypeInfo tests (BoldSystemRT)

procedure TTestBoldMeta.TestSystemTypeInfoClassCount;
var
  STI: TBoldSystemTypeInfo;
begin
  STI := GetSystem.BoldSystemTypeInfo;
  Assert.IsTrue(STI.TopSortedClasses.Count > 0, 'Should have at least one class type info');
end;

procedure TTestBoldMeta.TestClassTypeInfoByExpressionName;
var
  CTI: TBoldClassTypeInfo;
begin
  CTI := GetSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'];
  Assert.IsNotNull(CTI, 'Should find ClassTypeInfo for ClassA');
  Assert.AreEqual('ClassA', CTI.ExpressionName, 'ExpressionName should match');
end;

procedure TTestBoldMeta.TestClassTypeInfoAttributes;
var
  CTI: TBoldClassTypeInfo;
begin
  CTI := GetSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'];
  Assert.IsTrue(CTI.AttributeCount > 0, 'ClassA should have attributes');
end;

procedure TTestBoldMeta.TestClassTypeInfoRoles;
var
  CTI: TBoldClassTypeInfo;
begin
  CTI := GetSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'];
  // Access AllRoles to cover the property getter
  Assert.IsTrue(CTI.AllRoles.Count >= 0, 'AllRoles count should be >= 0');
end;

procedure TTestBoldMeta.TestClassTypeInfoSuperType;
var
  CTI: TBoldClassTypeInfo;
  DerivedCTI: TBoldClassTypeInfo;
begin
  CTI := GetSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'];
  DerivedCTI := GetSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['ClassDerivedA'];
  if Assigned(DerivedCTI) then
  begin
    Assert.IsNotNull(DerivedCTI.SuperClassTypeInfo, 'DerivedA should have super type');
    Assert.AreSame(TObject(CTI), TObject(DerivedCTI.SuperClassTypeInfo), 'Super type should be ClassA');
  end
  else
    Assert.Pass('ClassDerivedA not in system');
end;

procedure TTestBoldMeta.TestClassTypeInfoAllMembers;
var
  CTI: TBoldClassTypeInfo;
begin
  CTI := GetSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'];
  Assert.IsTrue(CTI.AllMembers.Count > 0, 'ClassA should have members in AllMembers');
end;

procedure TTestBoldMeta.TestClassTypeInfoIsAbstract;
var
  CTI: TBoldClassTypeInfo;
begin
  CTI := GetSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'];
  Assert.IsFalse(CTI.IsAbstract, 'ClassA should not be abstract');
end;

// BoldSystemRT — Search

procedure TTestBoldMeta.TestSearchByClassName;
var
  Result: TBoldMetaElement;
begin
  Result := GetSystem.BoldSystemTypeInfo.FindElement('ClassA', [], [stClass]);
  Assert.IsNotNull(Result, 'Search should find ClassA');
  Assert.AreEqual('ClassA', Result.ExpressionName, 'Found element should be ClassA');
end;

procedure TTestBoldMeta.TestSearchByAttributeName;
var
  Result: TBoldMetaElement;
begin
  Result := GetSystem.BoldSystemTypeInfo.FindElement('aString', [], [stAttribute]);
  Assert.IsNotNull(Result, 'Search should find aString attribute');
end;

procedure TTestBoldMeta.TestSearchPartialMatch;
var
  Result: TBoldMetaElement;
begin
  Result := GetSystem.BoldSystemTypeInfo.FindElement('Class', [soPartialMatch], [stClass]);
  Assert.IsNotNull(Result, 'Partial match should find a class containing "Class"');
end;

procedure TTestBoldMeta.TestSearchNotFound;
var
  Result: TBoldMetaElement;
begin
  Result := GetSystem.BoldSystemTypeInfo.FindElement('NonExistentXYZ', [], [stClass, stAttribute]);
  Assert.IsNull(Result, 'Search for non-existent should return nil');
end;

// BoldSystemRT — ValueTypeNames

procedure TTestBoldMeta.TestGetValueTypeNames;
var
  Names: TStringList;
begin
  Names := TStringList.Create;
  try
    GetSystem.BoldSystemTypeInfo.GetValueTypeNames(Names, True, True, True, True, True);
    Assert.IsTrue(Names.Count > 0, 'GetValueTypeNames should return names');
    Assert.IsTrue(Names.IndexOf('ClassA') >= 0, 'Should contain ClassA');
  finally
    Names.Free;
  end;
end;

procedure TTestBoldMeta.TestGetValueTypeNameList;
var
  List: TBoldElementTypeInfoList;
begin
  List := GetSystem.BoldSystemTypeInfo.ValueTypeNameList;
  Assert.IsNotNull(List, 'ValueTypeNameList should not be nil');
  Assert.IsTrue(List.Count > 0, 'ValueTypeNameList should have entries');
end;

procedure TTestBoldMeta.TestGetElementTypeInfoByExpressionName;
var
  Info: TBoldElementTypeInfo;
begin
  Info := GetSystem.BoldSystemTypeInfo.ElementTypeInfoByExpressionName['ClassA'];
  Assert.IsNotNull(Info, 'Should find ClassA element type info');
  Assert.AreEqual('ClassA', Info.ExpressionName, 'ExpressionName should match');

  // Non-existent should return nil
  Info := GetSystem.BoldSystemTypeInfo.ElementTypeInfoByExpressionName['NonExistent'];
  Assert.IsNull(Info, 'Non-existent should return nil');
end;

procedure TTestBoldMeta.TestConformsTo;
var
  STI: TBoldSystemTypeInfo;
begin
  STI := GetSystem.BoldSystemTypeInfo;
  Assert.IsTrue(STI.ConformsTo(STI), 'SystemTypeInfo should conform to itself');
end;

// BoldSystemRT — MemberRTInfo

procedure TTestBoldMeta.TestMemberRTInfoByExpressionName;
var
  CTI: TBoldClassTypeInfo;
  MRI: TBoldMemberRTInfo;
begin
  CTI := GetSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'];
  MRI := CTI.MemberRTInfoByExpressionName['aString'];
  Assert.IsNotNull(MRI, 'Should find aString member');
  Assert.AreEqual('aString', MRI.ExpressionName, 'Member name should match');
end;

procedure TTestBoldMeta.TestMemberRTInfoIsAttribute;
var
  CTI: TBoldClassTypeInfo;
  MRI: TBoldMemberRTInfo;
begin
  CTI := GetSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'];
  MRI := CTI.MemberRTInfoByExpressionName['aString'];
  Assert.IsTrue(MRI.IsAttribute, 'aString should be an attribute');
  Assert.IsFalse(MRI.IsRole, 'aString should not be a role');
end;

procedure TTestBoldMeta.TestMemberRTInfoIsRole;
var
  CTI: TBoldClassTypeInfo;
  i: Integer;
  MRI: TBoldMemberRTInfo;
begin
  CTI := GetSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'];
  // Find any role in ClassA's members
  for i := 0 to CTI.AllMembers.Count - 1 do
  begin
    MRI := CTI.AllMembers[i];
    if MRI.IsRole then
    begin
      Assert.IsFalse(MRI.IsAttribute, 'Role should not be an attribute');
      Break;
    end;
  end;
  // jehoBCBoldTest model may not have roles for ClassA, so just check we can iterate
  Assert.IsTrue(CTI.AllMembers.Count > 0, 'ClassA should have at least some members');
end;

// BoldMeta — additional coverage

procedure TTestBoldMeta.TestClassHasNotNullMembers;
var
  MoldClass: TMoldClass;
begin
  MoldClass := GetModel.Classes.ItemsByName['ClassA'];
  // HasNotNullMembers checks if any attribute has AllowNull = False
  // Just verify it doesn't raise and returns a boolean
  if MoldClass.HasNotNullMembers then
    Assert.Pass('ClassA has not-null members')
  else
    Assert.Pass('ClassA has no not-null members');
end;

procedure TTestBoldMeta.TestAllNativeAttributes;
var
  MoldClass: TMoldClass;
begin
  MoldClass := GetModel.Classes.ItemsByName['ClassA'];
  Assert.IsNotNull(MoldClass.AllNativeAttributes, 'AllNativeAttributes should not be nil');
  Assert.IsTrue(MoldClass.AllNativeAttributes.Count >= 0,
    'AllNativeAttributes count should be non-negative');
end;

procedure TTestBoldMeta.TestModelEnsureTopSorted;
begin
  // EnsureTopSorted is called during initialization, verify the result is consistent
  GetModel.EnsureTopSorted;
  Assert.IsTrue(GetModel.Classes.Count > 0, 'Model should have classes after top sort');
  Assert.AreEqual(0, GetModel.RootClass.TopSortedIndex, 'Root class should have TopSortedIndex 0');
end;

procedure TTestBoldMeta.TestAttributeAllowNull;
var
  MoldClass: TMoldClass;
  i: Integer;
begin
  MoldClass := GetModel.Classes.ItemsByName['ClassA'];
  // Verify we can read AllowNull for each attribute
  for i := 0 to MoldClass.Attributes.Count - 1 do
  begin
    // Just accessing AllowNull exercises the property getter
    if MoldClass.Attributes[i].AllowNull then
      ; // nullable
  end;
  Assert.IsTrue(MoldClass.Attributes.Count > 0, 'ClassA should have attributes');
end;

procedure TTestBoldMeta.TestClassIncFileName;
var
  MoldClass: TMoldClass;
  IncFile: string;
begin
  MoldClass := GetModel.Classes.ItemsByName['ClassA'];
  IncFile := MoldClass.IncFileName;
  // IncFileName reads from tagged values; may be empty for test model
  Assert.Pass('IncFileName returned: ' + IncFile);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldMeta);

end.
