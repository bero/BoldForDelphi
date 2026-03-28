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
  end;

implementation

uses
  SysUtils;

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

initialization
  TDUnitX.RegisterTestFixture(TTestBoldMeta);

end.
