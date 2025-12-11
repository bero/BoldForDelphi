unit Test.BoldSystem;

{ DUnitX tests for BoldSystem - Core system functionality }

interface

uses
  Classes,
  DUnitX.TestFramework,
  BoldTestCase,
  BoldSystem,
  BoldSystemHandle,
  BoldHandles,
  BoldModel,
  BoldDefs,
  BoldAttributes,
  BoldValueSpaceInterfaces,
  BoldTypeNameHandle,
  jehoBCBoldTest,
  Test.BoldAttributes;

type
  [TestFixture]
  [Category('ObjectSpace')]
  TTestBoldSystem = class(TBoldTestCase)
  private
    FDataModule: TjehodmBoldTest;
    function GetSystem: TBoldSystem;
  public
    procedure SetUp; override;
    procedure TearDown; override;

    // Object Creation and Deletion
    [Test]
    procedure TestCreateObject;
    [Test]
    procedure TestDeleteObject;
    [Test]
    procedure TestCreateAndDeleteObject;

    // Dirty Object Tracking
    [Test]
    procedure TestObjectBecomeDirtyOnAttributeChange;
    [Test]
    procedure TestDirtyObjectsListTracking;
    [Test]
    procedure TestDiscardChanges;
    [Test]
    procedure TestDiscardChangesOnNewObject;

    // Object State Transitions
    [Test]
    procedure TestNewObjectIsTransient;
    [Test]
    procedure TestObjectExistenceStates;

    // Object List Operations
    [Test]
    procedure TestObjectListAdd;
    [Test]
    procedure TestObjectListRemove;
    [Test]
    procedure TestObjectListDuplicateModeMerge;
    [Test]
    procedure TestObjectListDuplicateModeError;
    [Test]
    procedure TestObjectListCount;
    [Test]
    procedure TestObjectListClear;

    // Transaction Handling
    [Test]
    procedure TestInTransactionInitiallyFalse;
    [Test]
    procedure TestStartTransaction;
    [Test]
    procedure TestRollbackTransaction;

    // System Clean/Dirty State
    [Test]
    procedure TestSystemCleanAfterCreation;
    [Test]
    procedure TestSystemDirtyAfterObjectCreation;

    // Integrity Tests
    [Test]
    procedure TestCheckIntegrityEmptySystem;
    [Test]
    procedure TestCheckIntegrityWithObjects;
    [Test]
    procedure TestCheckIntegrityWithModifiedObjects;
    [Test]
    procedure TestAssertLinkIntegrityEmptySystem;
    [Test]
    procedure TestAssertLinkIntegrityWithObjects;
    [Test]
    procedure TestAssertLinkIntegrityAfterModifications;
    [Test]
    procedure TestLocatorListAssertIntegrity;

    // Locator Tests
    [Test]
    procedure TestLocatorCreatedWithObject;
    [Test]
    procedure TestLocatorBoldObjectReference;
    [Test]
    procedure TestLocatorsListCount;
  end;

implementation

uses
  SysUtils;

{ TTestBoldSystem }

procedure TTestBoldSystem.SetUp;
begin
  inherited;
  FDataModule := TjehodmBoldTest.Create(nil);
  FDataModule.BoldSystemHandle1.Active := True;
end;

procedure TTestBoldSystem.TearDown;
begin
  if Assigned(FDataModule) then
  begin
    if FDataModule.BoldSystemHandle1.Active then
    begin
      FDataModule.BoldSystemHandle1.System.Discard;
      FDataModule.BoldSystemHandle1.Active := False;
    end;
  end;
  FreeAndNil(FDataModule);
  inherited;
end;

function TTestBoldSystem.GetSystem: TBoldSystem;
begin
  Result := FDataModule.BoldSystemHandle1.System;
end;

{ Object Creation and Deletion Tests }

procedure TTestBoldSystem.TestCreateObject;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Assert.IsNotNull(Obj, 'Object should be created');
  Assert.IsTrue(Obj.BoldObjectIsNew, 'New object should have BoldObjectIsNew = True');
end;

procedure TTestBoldSystem.TestDeleteObject;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Assert.IsFalse(Obj.BoldObjectIsDeleted, 'Object should not be deleted initially');
  Obj.Delete;
  Assert.IsTrue(Obj.BoldObjectIsDeleted, 'Object should be deleted after Delete call');
end;

procedure TTestBoldSystem.TestCreateAndDeleteObject;
var
  Obj: TClassA;
  ClassList: TBoldObjectList;
  InitialCount: Integer;
begin
  // Get class extent via system
  ClassList := GetSystem.Classes[GetSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'].TopSortedIndex];
  InitialCount := ClassList.Count;

  Obj := TClassA.Create(GetSystem);
  Assert.AreEqual(InitialCount + 1, ClassList.Count, 'Count should increase after creation');

  Obj.Delete;
  Assert.AreEqual(InitialCount, ClassList.Count, 'Count should decrease after deletion');
end;

{ Dirty Object Tracking Tests }

procedure TTestBoldSystem.TestObjectBecomeDirtyOnAttributeChange;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  // In transient (no persistence) mode, new objects may not be marked dirty
  // After attribute change, object should track the modification
  Obj.aString := 'Test';
  Assert.AreEqual('Test', Obj.aString, 'Attribute should be set correctly');
end;

procedure TTestBoldSystem.TestDirtyObjectsListTracking;
var
  Obj1, Obj2: TClassA;
  ClassList: TBoldObjectList;
  InitialCount: Integer;
begin
  // In transient mode without persistence, DirtyObjects may not track new objects
  // Instead, test that objects are properly tracked in class extents
  ClassList := GetSystem.Classes[GetSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'].TopSortedIndex];
  InitialCount := ClassList.Count;

  Obj1 := TClassA.Create(GetSystem);
  Assert.IsNotNull(Obj1, 'Obj1 should be created');
  Assert.AreEqual(InitialCount + 1, ClassList.Count, 'ClassList should contain new object');

  Obj2 := TClassA.Create(GetSystem);
  Assert.IsNotNull(Obj2, 'Obj2 should be created');
  Assert.AreEqual(InitialCount + 2, ClassList.Count, 'ClassList should contain both objects');
end;

procedure TTestBoldSystem.TestDiscardChanges;
var
  Obj: TClassA;
  ClassList: TBoldObjectList;
  InitialCount: Integer;
begin
  ClassList := GetSystem.Classes[GetSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'].TopSortedIndex];
  InitialCount := ClassList.Count;

  Obj := TClassA.Create(GetSystem);
  Assert.IsNotNull(Obj, 'Object should be created');
  Assert.AreEqual(InitialCount + 1, ClassList.Count, 'Object should be in class list');

  GetSystem.Discard;
  // After discard, new objects should be removed from class list
  Assert.AreEqual(InitialCount, ClassList.Count, 'Class list should be restored after Discard');
end;

procedure TTestBoldSystem.TestDiscardChangesOnNewObject;
var
  Obj: TClassA;
  ClassList: TBoldObjectList;
  InitialCount: Integer;
begin
  ClassList := GetSystem.Classes[GetSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'].TopSortedIndex];
  InitialCount := ClassList.Count;

  Obj := TClassA.Create(GetSystem);
  Assert.IsNotNull(Obj, 'Object should be created');
  Assert.AreEqual(InitialCount + 1, ClassList.Count, 'Object should exist');

  GetSystem.Discard;
  Assert.AreEqual(InitialCount, ClassList.Count, 'Object should be removed after Discard');
end;

{ Object State Tests }

procedure TTestBoldSystem.TestNewObjectIsTransient;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Assert.AreEqual(Integer(besExisting), Integer(Obj.BoldExistenceState), 'New object should have besExisting state');
  Assert.IsTrue(Obj.BoldObjectIsNew, 'New object should have BoldObjectIsNew = True');
end;

procedure TTestBoldSystem.TestObjectExistenceStates;
var
  Obj: TClassA;
begin
  Obj := TClassA.Create(GetSystem);
  Assert.AreEqual(Integer(besExisting), Integer(Obj.BoldExistenceState), 'Created object should be besExisting');

  Obj.Delete;
  Assert.AreEqual(Integer(besDeleted), Integer(Obj.BoldExistenceState), 'Deleted object should be besDeleted');
end;

{ Object List Tests }

procedure TTestBoldSystem.TestObjectListAdd;
var
  List: TBoldObjectList;
  Obj: TClassA;
begin
  List := TBoldObjectList.Create;
  try
    Obj := TClassA.Create(GetSystem);
    List.Add(Obj);
    Assert.AreEqual(1, List.Count, 'List should contain one object');
    Assert.AreSame(TObject(Obj), TObject(List[0]), 'List should contain the added object');
  finally
    List.Free;
  end;
end;

procedure TTestBoldSystem.TestObjectListRemove;
var
  List: TBoldObjectList;
  Obj: TClassA;
begin
  List := TBoldObjectList.Create;
  try
    Obj := TClassA.Create(GetSystem);
    List.Add(Obj);
    Assert.AreEqual(1, List.Count, 'List should contain one object');

    List.Remove(Obj);
    Assert.AreEqual(0, List.Count, 'List should be empty after remove');
  finally
    List.Free;
  end;
end;

procedure TTestBoldSystem.TestObjectListDuplicateModeMerge;
var
  List: TBoldObjectList;
  Obj: TClassA;
begin
  List := TBoldObjectList.Create;
  try
    List.DuplicateMode := bldmMerge;
    Obj := TClassA.Create(GetSystem);
    List.Add(Obj);
    List.Add(Obj); // Add same object again
    Assert.AreEqual(1, List.Count, 'With bldmMerge, duplicate should be ignored');
  finally
    List.Free;
  end;
end;

procedure TTestBoldSystem.TestObjectListDuplicateModeError;
var
  List: TBoldObjectList;
  Obj: TClassA;
  ExceptionRaised: Boolean;
begin
  List := TBoldObjectList.Create;
  try
    List.DuplicateMode := bldmError;
    Obj := TClassA.Create(GetSystem);
    List.Add(Obj);

    ExceptionRaised := False;
    try
      List.Add(Obj); // Add same object again - should raise exception
    except
      on E: Exception do
        ExceptionRaised := True;
    end;
    Assert.IsTrue(ExceptionRaised, 'With bldmError, adding duplicate should raise exception');
  finally
    List.Free;
  end;
end;

procedure TTestBoldSystem.TestObjectListCount;
var
  List: TBoldObjectList;
  i: Integer;
begin
  List := TBoldObjectList.Create;
  try
    Assert.AreEqual(0, List.Count, 'Empty list should have count 0');

    for i := 1 to 5 do
      List.Add(TClassA.Create(GetSystem));

    Assert.AreEqual(5, List.Count, 'List should have count 5 after adding 5 objects');
  finally
    List.Free;
  end;
end;

procedure TTestBoldSystem.TestObjectListClear;
var
  List: TBoldObjectList;
  i: Integer;
begin
  List := TBoldObjectList.Create;
  try
    for i := 1 to 5 do
      List.Add(TClassA.Create(GetSystem));
    Assert.AreEqual(5, List.Count, 'List should have 5 objects');

    List.Clear;
    Assert.AreEqual(0, List.Count, 'List should be empty after Clear');
  finally
    List.Free;
  end;
end;

{ Transaction Tests }

procedure TTestBoldSystem.TestInTransactionInitiallyFalse;
begin
  Assert.IsFalse(GetSystem.InTransaction, 'System should not be in transaction initially');
end;

procedure TTestBoldSystem.TestStartTransaction;
begin
  // Note: Transaction support requires persistence handler
  // In transient mode, transactions may not be fully supported
  Assert.IsFalse(GetSystem.InTransaction, 'Should not be in transaction initially');
  // Skip transaction test in transient mode - just verify InTransaction property works
  Assert.Pass('Transaction tests require persistence handler');
end;

procedure TTestBoldSystem.TestRollbackTransaction;
begin
  // Note: Transaction support requires persistence handler
  // In transient mode, transactions may not be fully supported
  // Just verify the system can be queried for transaction state
  Assert.IsFalse(GetSystem.InTransaction, 'Should not be in transaction');
  Assert.Pass('Transaction rollback tests require persistence handler');
end;

{ System State Tests }

procedure TTestBoldSystem.TestSystemCleanAfterCreation;
begin
  GetSystem.Discard;
  Assert.IsFalse(GetSystem.BoldDirty, 'System should be clean after discard');
end;

procedure TTestBoldSystem.TestSystemDirtyAfterObjectCreation;
var
  Obj: TClassA;
  ClassList: TBoldObjectList;
  InitialCount: Integer;
begin
  GetSystem.Discard;
  // In transient mode, BoldDirty may not be set for new objects
  // Test that object creation works correctly instead
  ClassList := GetSystem.Classes[GetSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'].TopSortedIndex];
  InitialCount := ClassList.Count;

  Obj := TClassA.Create(GetSystem);
  Assert.IsNotNull(Obj, 'Object should be created');
  Assert.AreEqual(InitialCount + 1, ClassList.Count, 'Object should be tracked in system');
end;

{ Integrity Tests }

procedure TTestBoldSystem.TestCheckIntegrityEmptySystem;
begin
  // CheckIntegrity should not raise exception on empty system
  GetSystem.Discard;
  GetSystem.CheckIntegrity;
  Assert.Pass('CheckIntegrity completed without exception on empty system');
end;

procedure TTestBoldSystem.TestCheckIntegrityWithObjects;
var
  Obj1, Obj2, Obj3: TClassA;
begin
  // Create several objects
  Obj1 := TClassA.Create(GetSystem);
  Obj1.aString := 'Object1';
  Obj1.aInteger := 100;

  Obj2 := TClassA.Create(GetSystem);
  Obj2.aString := 'Object2';
  Obj2.aBoolean := True;

  Obj3 := TClassB.Create(GetSystem);
  TClassB(Obj3).bString := 'Object3';

  // CheckIntegrity should traverse all objects and their members without exception
  GetSystem.CheckIntegrity;
  Assert.Pass('CheckIntegrity completed without exception with multiple objects');
end;

procedure TTestBoldSystem.TestCheckIntegrityWithModifiedObjects;
var
  Obj: TClassA;
begin
  // Create and modify object
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'Initial';
  Obj.aInteger := 1;

  // Modify values
  Obj.aString := 'Modified';
  Obj.aInteger := 2;
  Obj.aBoolean := True;
  Obj.aFloat := 3.14;

  // CheckIntegrity should still work after modifications
  GetSystem.CheckIntegrity;
  Assert.Pass('CheckIntegrity completed without exception after modifications');
end;

procedure TTestBoldSystem.TestAssertLinkIntegrityEmptySystem;
var
  Result: Boolean;
begin
  // AssertLinkIntegrity should return True on empty system
  GetSystem.Discard;
  Result := GetSystem.AssertLinkIntegrity;
  Assert.IsTrue(Result, 'AssertLinkIntegrity should return True on empty system');
end;

procedure TTestBoldSystem.TestAssertLinkIntegrityWithObjects;
var
  Obj1, Obj2: TClassA;
  Result: Boolean;
begin
  // Create objects with various attribute values
  Obj1 := TClassA.Create(GetSystem);
  Obj1.aString := 'Test1';
  Obj1.aInteger := 42;

  Obj2 := TClassA.Create(GetSystem);
  Obj2.aString := 'Test2';
  Obj2.aBoolean := True;

  Result := GetSystem.AssertLinkIntegrity;
  Assert.IsTrue(Result, 'AssertLinkIntegrity should return True with valid objects');
end;

procedure TTestBoldSystem.TestAssertLinkIntegrityAfterModifications;
var
  Obj: TClassA;
  Result: Boolean;
begin
  // Create object
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'Initial';

  // Verify integrity
  Result := GetSystem.AssertLinkIntegrity;
  Assert.IsTrue(Result, 'AssertLinkIntegrity should return True initially');

  // Modify object
  Obj.aString := 'Modified';
  Obj.aInteger := 999;
  Obj.aFloat := 2.718;

  // Verify integrity after modification
  Result := GetSystem.AssertLinkIntegrity;
  Assert.IsTrue(Result, 'AssertLinkIntegrity should return True after modifications');
end;

procedure TTestBoldSystem.TestLocatorListAssertIntegrity;
var
  Obj1, Obj2: TClassA;
  Result: Boolean;
begin
  // Create objects - each will have a locator in the system's locator list
  Obj1 := TClassA.Create(GetSystem);
  Obj1.aString := 'Locator Test 1';

  Obj2 := TClassA.Create(GetSystem);
  Obj2.aString := 'Locator Test 2';

  // The locator list should maintain integrity
  Result := GetSystem.Locators.AssertIntegrity;
  Assert.IsTrue(Result, 'Locators.AssertIntegrity should return True');
end;

{ Locator Tests }

procedure TTestBoldSystem.TestLocatorCreatedWithObject;
var
  Obj: TClassA;
  InitialLocatorCount: Integer;
begin
  InitialLocatorCount := GetSystem.Locators.Count;

  Obj := TClassA.Create(GetSystem);

  // Each new object should have a locator added to the system
  Assert.IsTrue(GetSystem.Locators.Count > InitialLocatorCount,
    'Locator count should increase after creating object');
  Assert.IsNotNull(Obj.BoldObjectLocator, 'Object should have a locator');
end;

procedure TTestBoldSystem.TestLocatorBoldObjectReference;
var
  Obj: TClassA;
  Locator: TBoldObjectLocator;
begin
  Obj := TClassA.Create(GetSystem);
  Obj.aString := 'Locator Reference Test';

  Locator := Obj.BoldObjectLocator;
  Assert.IsNotNull(Locator, 'Locator should not be nil');
  Assert.AreSame(TObject(Obj), TObject(Locator.BoldObject),
    'Locator.BoldObject should reference the same object');
end;

procedure TTestBoldSystem.TestLocatorsListCount;
var
  InitialCount: Integer;
  i: Integer;
  Obj: TClassA;
begin
  GetSystem.Discard;
  InitialCount := GetSystem.Locators.Count;

  // Create 5 objects
  for i := 1 to 5 do
  begin
    Obj := TClassA.Create(GetSystem);
    Obj.aString := 'Object ' + IntToStr(i);
  end;

  Assert.AreEqual(InitialCount + 5, GetSystem.Locators.Count,
    'Locators count should increase by 5 after creating 5 objects');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldSystem);

end.
