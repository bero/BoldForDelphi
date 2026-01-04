unit Test.BoldListHandle;

interface

uses
  Classes,
  DUnitX.TestFramework,
  BoldSystem,
  BoldSystemRT,
  BoldElements,
  BoldListHandle,
  jehoBCBoldTest,
  Test.BoldAttributes;  // For TjehodmBoldTest

type
  [TestFixture]
  [Category('Handles')]
  TTestBoldListHandle = class
  private
    FDataModule: TjehodmBoldTest;
    function GetSystem: TBoldSystem;
    function GetListHandle: TBoldListHandle;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    // Tests for TBoldListHandle using DFM-configured handle
    [Test]
    [Category('Quick')]
    procedure TestListHandleNotNil;
    [Test]
    [Category('Quick')]
    procedure TestListHandleCountEmpty;
    [Test]
    [Category('Quick')]
    procedure TestListHandleCountWithObjects;
    [Test]
    [Category('Quick')]
    procedure TestListHandleObjectListProperty;
    [Test]
    [Category('Quick')]
    procedure TestListHandleFirst;
    [Test]
    [Category('Quick')]
    procedure TestListHandleLast;
    [Test]
    [Category('Quick')]
    procedure TestListHandleNavigation;
  end;

implementation

uses
  SysUtils,
  BoldDefs;

{ TTestBoldListHandle }

procedure TTestBoldListHandle.SetUp;
begin
  FDataModule := TjehodmBoldTest.Create(nil);
end;

procedure TTestBoldListHandle.TearDown;
begin
  FreeAndNil(FDataModule);
end;

function TTestBoldListHandle.GetSystem: TBoldSystem;
begin
  Result := FDataModule.BoldSystemHandle1.System;
end;

function TTestBoldListHandle.GetListHandle: TBoldListHandle;
begin
  Result := FDataModule.BoldListHandle1;
end;

procedure TTestBoldListHandle.TestListHandleNotNil;
begin
  // Ensure system is active by accessing it
  Assert.IsNotNull(GetSystem, 'System should be active');

  Assert.IsNotNull(GetListHandle, 'BoldListHandle1 should not be nil');
  Assert.IsNotNull(GetListHandle.StaticSystemHandle, 'StaticSystemHandle should be set');
  Assert.IsTrue(GetListHandle.Enabled, 'Handle should be enabled');
end;

procedure TTestBoldListHandle.TestListHandleCountEmpty;
begin
  // Ensure system is active
  Assert.IsNotNull(GetSystem, 'System should be active');
  // No objects created yet
  Assert.AreEqual(0, GetListHandle.Count, 'Count should be 0 when no objects exist');
end;

procedure TTestBoldListHandle.TestListHandleCountWithObjects;
var
  List: TBoldList;
  ClassTypeInfo: TBoldClassTypeInfo;
  DirectList: TBoldObjectList;
  DirectCount: Integer;
begin
  // Create some objects
  TClassA.Create(GetSystem);
  TClassA.Create(GetSystem);
  TClassA.Create(GetSystem);

  // Debug: Check system directly - are objects actually in the system?
  ClassTypeInfo := GetSystem.BoldSystemTypeInfo.TopSortedClasses.ItemsByExpressionName['ClassA'];
  DirectList := GetSystem.Classes[ClassTypeInfo.TopSortedIndex];
  DirectCount := DirectList.Count;  // <-- Breakpoint here, check DirectCount

  Assert.AreEqual(3, DirectCount, 'System should have 3 ClassA instances directly');

  // Now check via handle
  List := GetListHandle.List;  // <-- Breakpoint here, step into to see derivation
  Assert.IsNotNull(List, 'List should not be nil');
  Assert.AreEqual(3, List.Count, 'List should contain 3 objects');
  Assert.AreEqual(3, GetListHandle.Count, 'Count should be 3 after creating 3 objects');
end;

procedure TTestBoldListHandle.TestListHandleObjectListProperty;
begin
  // Create an object
  TClassA.Create(GetSystem);

  // Test the ObjectList property (exercises AsObjectList helper)
  Assert.IsNotNull(GetListHandle.ObjectList, 'ObjectList should not be nil');
  Assert.IsTrue(GetListHandle.ObjectList is TBoldObjectList, 'ObjectList should be TBoldObjectList');
  Assert.AreEqual(1, GetListHandle.ObjectList.Count, 'ObjectList should have 1 item');
end;

procedure TTestBoldListHandle.TestListHandleFirst;
begin
  TClassA.Create(GetSystem);
  TClassA.Create(GetSystem);

  GetListHandle.First;
  Assert.AreEqual(0, GetListHandle.CurrentIndex, 'First should set CurrentIndex to 0');
end;

procedure TTestBoldListHandle.TestListHandleLast;
begin
  TClassA.Create(GetSystem);
  TClassA.Create(GetSystem);
  TClassA.Create(GetSystem);

  GetListHandle.Last;
  Assert.AreEqual(2, GetListHandle.CurrentIndex, 'Last should set CurrentIndex to Count-1');
end;

procedure TTestBoldListHandle.TestListHandleNavigation;
begin
  TClassA.Create(GetSystem);
  TClassA.Create(GetSystem);

  GetListHandle.First;
  Assert.IsTrue(GetListHandle.HasNext, 'HasNext should be true at first item');
  Assert.IsFalse(GetListHandle.HasPrior, 'HasPrior should be false at first item');

  GetListHandle.Next;
  Assert.IsFalse(GetListHandle.HasNext, 'HasNext should be false at last item');
  Assert.IsTrue(GetListHandle.HasPrior, 'HasPrior should be true at last item');

  GetListHandle.Prior;
  Assert.AreEqual(0, GetListHandle.CurrentIndex, 'Prior should move back to index 0');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldListHandle);

end.
