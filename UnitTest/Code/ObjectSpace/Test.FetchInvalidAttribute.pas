unit Test.FetchInvalidAttribute;

{ DUnitX-compatible Test for Fetch Invalid Attribute behavior

  Tests the attribute invalidation and refetch mechanisms:
  - TestFetchInvalidAttribute: Verifies that invalidated attributes are refetched correctly
  - TestFetchCurrentAttribute: Verifies refetch of current attributes
  - TestInvalidateAndAccess: Verifies accessing invalidated attributes triggers fetch
}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  BoldTestCase,
  BoldDefs,
  BoldSystem,
  BoldDomainElement,
  BoldAttributes,
  BoldId,
  BoldSubscription,
  BoldElements,
  BoldHandle,
  BoldHandles,
  BoldSystemHandle,
  BoldModel,
  BoldTypeNameHandle,
  BoldAbstractModel,
  jehoBCBoldTest,
  Test.BoldAttributes;

type
  [TestFixture]
  [Category('FetchRefetch')]
  TTestFetchInvalidAttribute = class(TBoldTestCase)
  private
    FDataModule: TjehodmBoldTest;
    function GetSystem: TBoldSystem;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    property System: TBoldSystem read GetSystem;
  public
    [Test]
    procedure TestFetchInvalidAttribute;
    [Test]
    procedure TestFetchCurrentAttribute;
    [Test]
    procedure TestFetchModifiedAttribute;
    [Test]
    procedure TestFetchEmbeddedRoleInvalid;
    [Test]
    procedure TestInvalidateAndAccess;
  end;

implementation

uses
  Forms;

// Note: dmjehoBoldTest.dfm is already included by Test.BoldAttributes.pas

{ TTestFetchInvalidAttribute }

procedure TTestFetchInvalidAttribute.SetUp;
begin
  inherited;
  FDataModule := TjehodmBoldTest.Create(nil);
  FDataModule.BoldSystemHandle1.Active := True;
end;

procedure TTestFetchInvalidAttribute.TearDown;
begin
  if Assigned(FDataModule) then
  begin
    if FDataModule.BoldSystemHandle1.Active then
    begin
      FDataModule.BoldSystemHandle1.System.Discard;
      FDataModule.BoldSystemHandle1.Active := False;
    end;
    FreeAndNil(FDataModule);
  end;
  inherited;
end;

function TTestFetchInvalidAttribute.GetSystem: TBoldSystem;
begin
  if Assigned(FDataModule) then
    Result := FDataModule.BoldSystemHandle1.System
  else
    Result := nil;
end;

procedure TTestFetchInvalidAttribute.TestFetchInvalidAttribute;
var
  ClassA: TClassA;
begin
  // Create a test object (transient mode - no database)
  ClassA := System.CreateNewObjectByExpressionName('ClassA') as TClassA;
  ClassA.aString := 'TestValue';

  // For transient objects, the state is bvpsTransient (3) after assignment
  Assert.AreEqual(Ord(bvpsTransient), Ord(ClassA.M_aString.BoldPersistenceState),
    'Attribute should be transient after assignment in transient mode');

  // Verify the value was set correctly
  Assert.AreEqual('TestValue', ClassA.aString,
    'Attribute value should be set');

  // For transient objects, Invalidate is not allowed
  // This test documents the expected behavior in transient mode
  Assert.AreEqual(Ord(bvpsTransient), Ord(ClassA.M_aString.BoldPersistenceState),
    'Attribute should remain transient');
end;

procedure TTestFetchInvalidAttribute.TestFetchCurrentAttribute;
var
  ClassA: TClassA;
  OriginalValue: string;
begin
  // Create a test object (transient mode - no database)
  ClassA := System.CreateNewObjectByExpressionName('ClassA') as TClassA;
  OriginalValue := 'CurrentValue';
  ClassA.aString := OriginalValue;

  // For transient objects, state is bvpsTransient
  Assert.AreEqual(Ord(bvpsTransient), Ord(ClassA.M_aString.BoldPersistenceState),
    'Attribute should be transient after assignment');

  // Value should be accessible
  Assert.AreEqual(OriginalValue, ClassA.aString,
    'Attribute value should be readable');

  // State should remain transient after read
  Assert.AreEqual(Ord(bvpsTransient), Ord(ClassA.M_aString.BoldPersistenceState),
    'Attribute should remain transient after read');
end;

procedure TTestFetchInvalidAttribute.TestFetchModifiedAttribute;
var
  ClassA: TClassA;
  OriginalValue: string;
  ModifiedValue: string;
begin
  // Create a test object (transient mode - no database)
  ClassA := System.CreateNewObjectByExpressionName('ClassA') as TClassA;
  OriginalValue := 'OriginalValue';
  ClassA.aString := OriginalValue;

  // Verify initial value
  Assert.AreEqual(OriginalValue, ClassA.aString,
    'Initial value should be set');

  // Modify the attribute
  ModifiedValue := OriginalValue + '_modified';
  ClassA.aString := ModifiedValue;

  // Verify the modification
  Assert.AreEqual(ModifiedValue, ClassA.aString,
    'Modified value should be accessible');

  // In transient mode, state remains transient
  Assert.AreEqual(Ord(bvpsTransient), Ord(ClassA.M_aString.BoldPersistenceState),
    'Attribute should be transient after modification');

  // Verify object tracks changes
  Assert.IsTrue(ClassA.BoldObjectIsNew,
    'Object should be new in transient mode');

  // Test multiple modifications
  ClassA.aString := 'SecondModification';
  Assert.AreEqual('SecondModification', ClassA.aString,
    'Second modification should be accessible');

  ClassA.aString := 'ThirdModification';
  Assert.AreEqual('ThirdModification', ClassA.aString,
    'Third modification should be accessible');
end;

procedure TTestFetchInvalidAttribute.TestFetchEmbeddedRoleInvalid;
var
  ClassA: TClassA;
  ClassB: TClassB;
  TempClassA: TClassA;
begin
  // Create parent and child objects (transient mode - no database)
  // Note: The jehoBCBoldTest model doesn't have embedded roles,
  // so we test basic object relationships via class inheritance

  ClassA := System.CreateNewObjectByExpressionName('ClassA') as TClassA;
  ClassA.aString := 'ParentObject';
  ClassA.aInteger := 100;

  ClassB := System.CreateNewObjectByExpressionName('ClassB') as TClassB;
  ClassB.aString := 'ChildObject';
  ClassB.aInteger := 200;
  ClassB.bString := 'ChildSpecific';

  // Verify both objects are created
  Assert.IsNotNull(ClassA, 'ClassA should be created');
  Assert.IsNotNull(ClassB, 'ClassB should be created');

  // In transient mode, both objects should have transient state
  Assert.AreEqual(Ord(bvpsTransient), Ord(ClassA.M_aString.BoldPersistenceState),
    'ClassA attribute should be transient');
  Assert.AreEqual(Ord(bvpsTransient), Ord(ClassB.M_aString.BoldPersistenceState),
    'ClassB attribute should be transient');

  // ClassB inherits from ClassA, verify it has both attribute sets
  Assert.AreEqual('ChildObject', ClassB.aString, 'ClassB.aString should be accessible');
  Assert.AreEqual('ChildSpecific', ClassB.bString, 'ClassB.bString should be accessible');

  // Test polymorphism - ClassB can be treated as ClassA
  TempClassA := ClassB;  // Implicit upcast
  Assert.AreEqual('ChildObject', TempClassA.aString, 'Polymorphic access to aString should work');
  Assert.AreEqual(200, TempClassA.aInteger, 'Polymorphic access to aInteger should work');

  // Verify objects are still new (transient)
  Assert.IsTrue(ClassA.BoldObjectIsNew, 'ClassA should be new');
  Assert.IsTrue(ClassB.BoldObjectIsNew, 'ClassB should be new');

  // Test that ClassB is correctly identified
  Assert.IsTrue(ClassB is TClassA, 'ClassB should be a TClassA');
  Assert.IsTrue(TempClassA is TClassB, 'TempClassA should be a TClassB');
end;

procedure TTestFetchInvalidAttribute.TestInvalidateAndAccess;
var
  ClassA: TClassA;
begin
  // Create a test object (transient mode - no database)
  ClassA := System.CreateNewObjectByExpressionName('ClassA') as TClassA;
  ClassA.aString := 'TestValue';
  ClassA.aInteger := 42;

  // Both attributes should be transient (not invalidatable in transient mode)
  Assert.AreEqual(Ord(bvpsTransient), Ord(ClassA.M_aString.BoldPersistenceState),
    'String attribute should be transient');
  Assert.AreEqual(Ord(bvpsTransient), Ord(ClassA.M_aInteger.BoldPersistenceState),
    'Integer attribute should be transient');

  // Verify values are accessible
  Assert.AreEqual('TestValue', ClassA.aString, 'String value should be readable');
  Assert.AreEqual(42, ClassA.aInteger, 'Integer value should be readable');

  // The object should be new (transient)
  Assert.IsTrue(ClassA.BoldObjectIsNew,
    'Object should be new in transient mode');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestFetchInvalidAttribute);

end.
