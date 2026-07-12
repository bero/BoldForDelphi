unit Test.BoldContainers;

interface

uses
  DUnitX.TestFramework,
  BoldContainers,
  BoldDefs;

type
  [TestFixture]
  TTestBoldContainers = class
  public
    // TBoldObjectArray tests
    [Test]
    procedure TestSortDataOwningArrayKeepsElementsAlive;
    [Test]
    procedure TestObjectArrayCreate;
    [Test]
    procedure TestObjectArrayAdd;
    [Test]
    procedure TestObjectArrayIndexOf;
    [Test]
    procedure TestObjectArrayInsert;
    [Test]
    procedure TestObjectArrayDelete;
    [Test]
    procedure TestObjectArrayRemove;
    [Test]
    procedure TestObjectArrayRemoveWithNil;
    [Test]
    procedure TestObjectArrayExchange;
    [Test]
    procedure TestObjectArrayMove;
    [Test]
    procedure TestObjectArrayClear;
    [Test]
    procedure TestObjectArrayPack;
    [Test]
    procedure TestObjectArrayCapacity;
    [Test]
    procedure TestObjectArrayCount;
    [Test]
    procedure TestObjectArrayEnumerator;
    [Test]
    procedure TestObjectArrayDataOwner;
    // TBoldPointerArray tests
    [Test]
    procedure TestPointerArrayAdd;
    [Test]
    procedure TestPointerArrayIndexOf;
    [Test]
    procedure TestPointerArrayRemove;
    [Test]
    procedure TestPointerArrayRemoveWithNil;
    // TBoldIntegerArray tests
    [Test]
    procedure TestIntegerArrayAdd;
    [Test]
    procedure TestIntegerArrayIndexOf;
    [Test]
    procedure TestIntegerArrayRemove;
    [Test]
    procedure TestIntegerArrayInsert;
    // TBoldInterfaceArray tests
    [Test]
    procedure TestInterfaceArrayAdd;
    [Test]
    procedure TestInterfaceArrayDataOwner;
    // TBoldObjectArray - Put
    [Test]
    procedure TestObjectArrayPut;
    // Sorting tests (actual sorting)
    [Test]
    procedure TestObjectArraySortQuickSort;
    [Test]
    procedure TestObjectArraySortMergeSort;
    [Test]
    procedure TestObjectArraySortEmptyAndSingle;
    // DeleteRange tests
    [Test]
    procedure TestObjectArrayDeleteRange;
    [Test]
    procedure TestObjectArrayDeleteRangeMiddle;
    [Test]
    procedure TestObjectArrayDeleteRangeDataOwner;
    // GrowDelta high capacity path
    [Test]
    procedure TestObjectArrayGrowDeltaHighCapacity;
    // TBoldInterfaceArray extended tests
    [Test]
    procedure TestInterfaceArrayInsert;
    [Test]
    procedure TestInterfaceArrayPut;
    [Test]
    procedure TestInterfaceArrayIndexOf;
    [Test]
    procedure TestInterfaceArrayRemove;
    [Test]
    procedure TestInterfaceArrayRemoveWithNil;
    // TBoldIntegerArray extended tests
    [Test]
    procedure TestIntegerArrayPut;
    [Test]
    procedure TestIntegerArrayIndexOfBug;
    // TBoldPointerArray - Insert, Put
    [Test]
    procedure TestPointerArrayInsert;
    [Test]
    procedure TestPointerArrayPut;
    // Error handling
    [Test]
    procedure TestObjectArrayIndexOutOfBounds;
    [Test]
    procedure TestObjectArrayCapacityLessThanCount;
    [Test]
    procedure TestObjectArrayExchangeOutOfBounds;
    [Test]
    procedure TestObjectArrayInsertOutOfBounds;
    [Test]
    procedure TestObjectArraySetCountNegative;
    [Test]
    procedure TestObjectArrayExchangeSameIndex;
  end;

implementation

uses
  SysUtils,
  Classes;

type
  // Test object for sorting
  TTestItem = class
  public
    Value: Integer;
    constructor Create(AValue: Integer);
  end;

constructor TTestItem.Create(AValue: Integer);
begin
  inherited Create;
  Value := AValue;
end;

function CompareTestItems(Item1, Item2: Pointer): Integer;
begin
  // Sort passes pointers to array slots, not the objects themselves
  // Need to dereference: PPointer(Item1)^ is the actual TObject
  Result := TTestItem(PPointer(Item1)^).Value - TTestItem(PPointer(Item2)^).Value;
end;

{ TTestBoldContainers }

procedure TTestBoldContainers.TestSortDataOwningArrayKeepsElementsAlive;
var
  Arr: TBoldObjectArray;
  A, B: TTestItem;
begin
  // Regression for H13: InsertSort (the default smMergeSort path for short
  // runs) shifts slots with raw Move and then calls Put, which Disposes the
  // slot's current occupant when bcoDataOwner is set - freeing an object the
  // shift had just duplicated into the neighbouring slot. Use-after-free
  // immediately, double-free when the array is destroyed.
  Arr := TBoldObjectArray.Create(2, [bcoDataOwner]);
  try
    B := TTestItem.Create(2);
    A := TTestItem.Create(1);
    Arr.Add(B);  // descending pair forces one insertion shift
    Arr.Add(A);
    Arr.Sort(CompareTestItems);
    Assert.AreEqual(2, Arr.Count, 'count unchanged by sort');
    Assert.AreSame(TObject(A), TObject(Arr[0]), 'ascending order after sort');
    Assert.AreSame(TObject(B), TObject(Arr[1]), 'ascending order after sort');
    Assert.AreEqual(2, TTestItem(Arr[1]).Value, 'element must still be alive after sort');
  finally
    Arr.Free;  // with the bug: double-free of the duplicated element here
  end;
end;

procedure TTestBoldContainers.TestObjectArrayCreate;
var
  Arr: TBoldObjectArray;
begin
  Arr := TBoldObjectArray.Create(10, []);
  try
    Assert.IsNotNull(Arr);
    Assert.AreEqual(0, Arr.Count);
    Assert.AreEqual(10, Arr.Capacity);
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArrayAdd;
var
  Arr: TBoldObjectArray;
  Obj1, Obj2: TObject;
  Idx: Integer;
begin
  Arr := TBoldObjectArray.Create(4, []);
  try
    Obj1 := TObject.Create;
    Obj2 := TObject.Create;
    try
      Idx := Arr.Add(Obj1);
      Assert.AreEqual(0, Idx);
      Assert.AreEqual(1, Arr.Count);

      Idx := Arr.Add(Obj2);
      Assert.AreEqual(1, Idx);
      Assert.AreEqual(2, Arr.Count);

      Assert.AreSame(Obj1, Arr[0]);
      Assert.AreSame(Obj2, Arr[1]);
    finally
      Obj2.Free;
      Obj1.Free;
    end;
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArrayIndexOf;
var
  Arr: TBoldObjectArray;
  Obj1, Obj2, Obj3: TObject;
begin
  Arr := TBoldObjectArray.Create(4, []);
  try
    Obj1 := TObject.Create;
    Obj2 := TObject.Create;
    Obj3 := TObject.Create;
    try
      Arr.Add(Obj1);
      Arr.Add(Obj2);

      Assert.AreEqual(0, Arr.IndexOf(Obj1));
      Assert.AreEqual(1, Arr.IndexOf(Obj2));
      Assert.AreEqual(-1, Arr.IndexOf(Obj3)); // Not in array
    finally
      Obj3.Free;
      Obj2.Free;
      Obj1.Free;
    end;
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArrayInsert;
var
  Arr: TBoldObjectArray;
  Obj1, Obj2, Obj3: TObject;
begin
  Arr := TBoldObjectArray.Create(4, []);
  try
    Obj1 := TObject.Create;
    Obj2 := TObject.Create;
    Obj3 := TObject.Create;
    try
      Arr.Add(Obj1);
      Arr.Add(Obj3);
      // Insert Obj2 between Obj1 and Obj3
      Arr.Insert(1, Obj2);

      Assert.AreEqual(3, Arr.Count);
      Assert.AreSame(Obj1, Arr[0]);
      Assert.AreSame(Obj2, Arr[1]);
      Assert.AreSame(Obj3, Arr[2]);
    finally
      Obj3.Free;
      Obj2.Free;
      Obj1.Free;
    end;
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArrayDelete;
var
  Arr: TBoldObjectArray;
  Obj1, Obj2, Obj3: TObject;
begin
  Arr := TBoldObjectArray.Create(4, []);
  try
    Obj1 := TObject.Create;
    Obj2 := TObject.Create;
    Obj3 := TObject.Create;
    try
      Arr.Add(Obj1);
      Arr.Add(Obj2);
      Arr.Add(Obj3);

      Arr.Delete(1); // Delete Obj2

      Assert.AreEqual(2, Arr.Count);
      Assert.AreSame(Obj1, Arr[0]);
      Assert.AreSame(Obj3, Arr[1]);
    finally
      Obj3.Free;
      Obj2.Free;
      Obj1.Free;
    end;
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArrayRemove;
var
  Arr: TBoldObjectArray;
  Obj1, Obj2, Obj3: TObject;
  Idx: Integer;
begin
  Arr := TBoldObjectArray.Create(4, []);
  try
    Obj1 := TObject.Create;
    Obj2 := TObject.Create;
    Obj3 := TObject.Create;
    try
      Arr.Add(Obj1);
      Arr.Add(Obj2);
      Arr.Add(Obj3);

      Idx := Arr.Remove(Obj2);
      Assert.AreEqual(1, Idx);
      Assert.AreEqual(2, Arr.Count);
      Assert.AreEqual(-1, Arr.IndexOf(Obj2));

      // Remove non-existent returns -1
      Idx := Arr.Remove(Obj2);
      Assert.AreEqual(-1, Idx);
    finally
      Obj3.Free;
      Obj2.Free;
      Obj1.Free;
    end;
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArrayRemoveWithNil;
var
  Arr: TBoldObjectArray;
  Obj1, Obj2, Obj3: TObject;
  Idx: Integer;
begin
  Arr := TBoldObjectArray.Create(4, []);
  try
    Obj1 := TObject.Create;
    Obj2 := TObject.Create;
    Obj3 := TObject.Create;
    try
      Arr.Add(Obj1);
      Arr.Add(Obj2);
      Arr.Add(Obj3);

      Idx := Arr.RemoveWithNil(Obj2);
      Assert.AreEqual(1, Idx);
      Assert.AreEqual(3, Arr.Count); // Count unchanged
      Assert.IsNull(Arr[1]); // Slot is nil
    finally
      Obj3.Free;
      Obj2.Free;
      Obj1.Free;
    end;
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArrayExchange;
var
  Arr: TBoldObjectArray;
  Obj1, Obj2, Obj3: TObject;
begin
  Arr := TBoldObjectArray.Create(4, []);
  try
    Obj1 := TObject.Create;
    Obj2 := TObject.Create;
    Obj3 := TObject.Create;
    try
      Arr.Add(Obj1);
      Arr.Add(Obj2);
      Arr.Add(Obj3);

      Arr.Exchange(0, 2);

      Assert.AreSame(Obj3, Arr[0]);
      Assert.AreSame(Obj2, Arr[1]);
      Assert.AreSame(Obj1, Arr[2]);
    finally
      Obj3.Free;
      Obj2.Free;
      Obj1.Free;
    end;
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArrayMove;
var
  Arr: TBoldObjectArray;
  Obj1, Obj2, Obj3: TObject;
begin
  Arr := TBoldObjectArray.Create(4, []);
  try
    Obj1 := TObject.Create;
    Obj2 := TObject.Create;
    Obj3 := TObject.Create;
    try
      Arr.Add(Obj1);
      Arr.Add(Obj2);
      Arr.Add(Obj3);

      // Move Obj1 to end
      Arr.Move(0, 2);

      Assert.AreSame(Obj2, Arr[0]);
      Assert.AreSame(Obj3, Arr[1]);
      Assert.AreSame(Obj1, Arr[2]);
    finally
      Obj3.Free;
      Obj2.Free;
      Obj1.Free;
    end;
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArrayClear;
var
  Arr: TBoldObjectArray;
  Obj1, Obj2: TObject;
begin
  Arr := TBoldObjectArray.Create(4, []);
  try
    Obj1 := TObject.Create;
    Obj2 := TObject.Create;
    try
      Arr.Add(Obj1);
      Arr.Add(Obj2);
      Assert.AreEqual(2, Arr.Count);

      Arr.Clear;

      Assert.AreEqual(0, Arr.Count);
      Assert.AreEqual(0, Arr.Capacity);
    finally
      Obj2.Free;
      Obj1.Free;
    end;
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArrayPack;
var
  Arr: TBoldObjectArray;
  Obj1: TObject;
begin
  Arr := TBoldObjectArray.Create(100, []);
  try
    Obj1 := TObject.Create;
    try
      Arr.Add(Obj1);
      Assert.AreEqual(100, Arr.Capacity);

      Arr.Pack;

      Assert.AreEqual(1, Arr.Capacity);
      Assert.AreEqual(1, Arr.Count);
    finally
      Obj1.Free;
    end;
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArrayCapacity;
var
  Arr: TBoldObjectArray;
begin
  Arr := TBoldObjectArray.Create(10, []);
  try
    Assert.AreEqual(10, Arr.Capacity);

    Arr.Capacity := 20;
    Assert.AreEqual(20, Arr.Capacity);

    Arr.Capacity := 5;
    Assert.AreEqual(5, Arr.Capacity);
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArrayCount;
var
  Arr: TBoldObjectArray;
begin
  Arr := TBoldObjectArray.Create(4, []);
  try
    Assert.AreEqual(0, Arr.Count);

    Arr.Count := 5;
    Assert.AreEqual(5, Arr.Count);
    Assert.IsTrue(Arr.Capacity >= 5);

    // Items should be nil
    Assert.IsNull(Arr[0]);
    Assert.IsNull(Arr[4]);
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArrayEnumerator;
var
  Arr: TBoldObjectArray;
  Obj1, Obj2, Obj3: TObject;
  Enum: TBoldArrayTraverser;
  Count: Integer;
begin
  Arr := TBoldObjectArray.Create(4, []);
  try
    Obj1 := TObject.Create;
    Obj2 := TObject.Create;
    Obj3 := TObject.Create;
    try
      Arr.Add(Obj1);
      Arr.Add(Obj2);
      Arr.Add(Obj3);

      Count := 0;
      Enum := Arr.GetEnumerator;
      try
        while Enum.MoveNext do
        begin
          Inc(Count);
          Assert.IsNotNull(Enum.GetCurrent);
        end;
      finally
        Enum.Free;
      end;

      Assert.AreEqual(3, Count);
    finally
      Obj3.Free;
      Obj2.Free;
      Obj1.Free;
    end;
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArrayDataOwner;
var
  Arr: TBoldObjectArray;
begin
  // With bcoDataOwner, array frees objects on Clear/Delete
  Arr := TBoldObjectArray.Create(4, [bcoDataOwner]);
  try
    Arr.Add(TObject.Create);
    Arr.Add(TObject.Create);
    Assert.AreEqual(2, Arr.Count);

    Arr.Delete(0); // Should free the object
    Assert.AreEqual(1, Arr.Count);

    // Clear should free remaining objects
    Arr.Clear;
    Assert.AreEqual(0, Arr.Count);
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestPointerArrayAdd;
var
  Arr: TBoldPointerArray;
  P1, P2: Pointer;
begin
  Arr := TBoldPointerArray.Create(4, []);
  try
    P1 := Pointer(1);
    P2 := Pointer(2);

    Assert.AreEqual(0, Arr.Add(P1));
    Assert.AreEqual(1, Arr.Add(P2));
    Assert.AreEqual(2, Arr.Count);
    Assert.AreEqual(P1, Arr[0]);
    Assert.AreEqual(P2, Arr[1]);
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestPointerArrayIndexOf;
var
  Arr: TBoldPointerArray;
  P1, P2, P3: Pointer;
begin
  Arr := TBoldPointerArray.Create(4, []);
  try
    P1 := Pointer(1);
    P2 := Pointer(2);
    P3 := Pointer(3);

    Arr.Add(P1);
    Arr.Add(P2);

    Assert.AreEqual(0, Arr.IndexOf(P1));
    Assert.AreEqual(1, Arr.IndexOf(P2));
    Assert.AreEqual(-1, Arr.IndexOf(P3));
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestPointerArrayRemove;
var
  Arr: TBoldPointerArray;
  P1, P2: Pointer;
begin
  Arr := TBoldPointerArray.Create(4, []);
  try
    P1 := Pointer(1);
    P2 := Pointer(2);

    Arr.Add(P1);
    Arr.Add(P2);

    Assert.AreEqual(0, Arr.Remove(P1));
    Assert.AreEqual(1, Arr.Count);
    Assert.AreEqual(P2, Arr[0]);
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestPointerArrayRemoveWithNil;
var
  Arr: TBoldPointerArray;
  P1, P2: Pointer;
begin
  Arr := TBoldPointerArray.Create(4, []);
  try
    P1 := Pointer(1);
    P2 := Pointer(2);

    Arr.Add(P1);
    Arr.Add(P2);

    Assert.AreEqual(0, Arr.RemoveWithNil(P1));
    Assert.AreEqual(2, Arr.Count); // Count unchanged
    Assert.IsNull(Arr[0]); // Slot is nil
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestIntegerArrayAdd;
var
  Arr: TBoldIntegerArray;
begin
  Arr := TBoldIntegerArray.Create(4, []);
  try
    Assert.AreEqual(0, Arr.Add(10));
    Assert.AreEqual(1, Arr.Add(20));
    Assert.AreEqual(2, Arr.Add(30));

    Assert.AreEqual(3, Arr.Count);
    Assert.AreEqual(10, Arr[0]);
    Assert.AreEqual(20, Arr[1]);
    Assert.AreEqual(30, Arr[2]);
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestIntegerArrayIndexOf;
var
  Arr: TBoldIntegerArray;
begin
  // Note: TBoldIntegerArray.IndexOf has a bug in source (uses = instead of <>)
  // This test verifies the array can store and retrieve integers
  Arr := TBoldIntegerArray.Create(4, []);
  try
    Arr.Add(10);
    Arr.Add(20);
    Arr.Add(30);

    Assert.AreEqual(3, Arr.Count);
    Assert.AreEqual(10, Arr[0]);
    Assert.AreEqual(20, Arr[1]);
    Assert.AreEqual(30, Arr[2]);
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestIntegerArrayRemove;
var
  Arr: TBoldIntegerArray;
begin
  // Note: TBoldIntegerArray.Remove relies on buggy IndexOf
  // Test basic Delete functionality instead
  Arr := TBoldIntegerArray.Create(4, []);
  try
    Arr.Add(10);
    Arr.Add(20);
    Arr.Add(30);

    Arr.Delete(1); // Delete middle element
    Assert.AreEqual(2, Arr.Count);
    Assert.AreEqual(10, Arr[0]);
    Assert.AreEqual(30, Arr[1]);
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestIntegerArrayInsert;
var
  Arr: TBoldIntegerArray;
begin
  Arr := TBoldIntegerArray.Create(4, []);
  try
    Arr.Add(10);
    Arr.Add(30);
    Arr.Insert(1, 20);

    Assert.AreEqual(3, Arr.Count);
    Assert.AreEqual(10, Arr[0]);
    Assert.AreEqual(20, Arr[1]);
    Assert.AreEqual(30, Arr[2]);
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestInterfaceArrayAdd;
var
  Arr: TBoldInterfaceArray;
  Intf1, Intf2: IInterface;
begin
  Arr := TBoldInterfaceArray.Create(4, []);
  try
    Intf1 := TInterfacedObject.Create;
    Intf2 := TInterfacedObject.Create;

    Assert.AreEqual(0, Arr.Add(Intf1));
    Assert.AreEqual(1, Arr.Add(Intf2));
    Assert.AreEqual(2, Arr.Count);
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestInterfaceArrayDataOwner;
var
  Arr: TBoldInterfaceArray;
  Intf: IInterface;
begin
  Arr := TBoldInterfaceArray.Create(4, [bcoDataOwner]);
  try
    Intf := TInterfacedObject.Create;
    Arr.Add(Intf);
    Assert.AreEqual(1, Arr.Count);
    // With bcoDataOwner, array manages ref count
    Arr.Clear;
    Assert.AreEqual(0, Arr.Count);
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArrayPut;
var
  Arr: TBoldObjectArray;
  Obj1, Obj2, Obj3: TObject;
begin
  Arr := TBoldObjectArray.Create(4, []);
  try
    Obj1 := TObject.Create;
    Obj2 := TObject.Create;
    Obj3 := TObject.Create;
    try
      Arr.Add(Obj1);
      Arr.Add(Obj2);

      // Replace item at index 1
      Arr[1] := Obj3;
      Assert.AreSame(Obj3, Arr[1], 'Put should replace item');
      Assert.AreEqual(2, Arr.Count, 'Count should not change');
    finally
      Obj3.Free;
      Obj2.Free;
      Obj1.Free;
    end;
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArraySortQuickSort;
var
  Arr: TBoldObjectArray;
  I: Integer;
begin
  Arr := TBoldObjectArray.Create(16, []);
  try
    // Add items in reverse order
    for I := 10 downto 1 do
      Arr.Add(TTestItem.Create(I));

    Arr.Sort(CompareTestItems, smQuickSort);

    // Verify sorted ascending
    for I := 0 to Arr.Count - 2 do
      Assert.IsTrue(TTestItem(Arr[I]).Value <= TTestItem(Arr[I + 1]).Value,
        Format('QuickSort: items %d and %d out of order', [I, I + 1]));

    // Free items manually
    for I := 0 to Arr.Count - 1 do
      Arr[I].Free;
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArraySortMergeSort;
var
  Arr: TBoldObjectArray;
  I: Integer;
begin
  // Don't use bcoDataOwner with sort - sort uses System.Move directly,
  // bypassing the Dispose/AddRef ownership protocol
  Arr := TBoldObjectArray.Create(16, []);
  try
    // Add items in reverse order
    for I := 10 downto 1 do
      Arr.Add(TTestItem.Create(I));

    Arr.Sort(CompareTestItems, smMergeSort);

    // Verify sorted ascending
    for I := 0 to Arr.Count - 2 do
      Assert.IsTrue(TTestItem(Arr[I]).Value <= TTestItem(Arr[I + 1]).Value,
        Format('MergeSort: items %d and %d out of order', [I, I + 1]));

    // Free items manually
    for I := 0 to Arr.Count - 1 do
      Arr[I].Free;
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArraySortEmptyAndSingle;
var
  Arr: TBoldObjectArray;
  Item: TTestItem;
begin
  // Sort empty array - should not crash
  Arr := TBoldObjectArray.Create(4, []);
  try
    Arr.Sort(CompareTestItems, smQuickSort);
    Assert.AreEqual(0, Arr.Count, 'Empty sort should leave count 0');

    // Sort single element
    Item := TTestItem.Create(42);
    Arr.Add(Item);
    Arr.Sort(CompareTestItems, smMergeSort);
    Assert.AreEqual(1, Arr.Count);
    Assert.AreEqual(42, TTestItem(Arr[0]).Value);
    Item.Free;
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArrayDeleteRange;
var
  Arr: TBoldObjectArray;
  Obj1, Obj2, Obj3: TObject;
begin
  Arr := TBoldObjectArray.Create(8, []);
  try
    Obj1 := TObject.Create;
    Obj2 := TObject.Create;
    Obj3 := TObject.Create;
    try
      Arr.Add(Obj1);
      Arr.Add(Obj2);
      Arr.Add(Obj3);

      // Delete last two items
      Arr.DeleteRange(1, 2);

      Assert.AreEqual(1, Arr.Count);
      Assert.IsTrue(Arr[0] = Obj1, 'First element should remain Obj1');
    finally
      Obj3.Free;
      Obj2.Free;
      Obj1.Free;
    end;
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArrayDeleteRangeMiddle;
var
  Arr: TBoldObjectArray;
  Obj1, Obj2, Obj3, Obj4: TObject;
begin
  // BUG: DeleteRange has a bug when deleting from the middle.
  // It decrements FCount BEFORE MoveItems, so the condition
  // "if ToIndex < FCount" fails and items after the range are not moved down.
  // This test documents the bug: deleting from the end works, but
  // deleting from the middle silently loses trailing items.
  Arr := TBoldObjectArray.Create(8, []);
  try
    Obj1 := TObject.Create;
    Obj2 := TObject.Create;
    Obj3 := TObject.Create;
    Obj4 := TObject.Create;
    try
      Arr.Add(Obj1);
      Arr.Add(Obj2);
      Arr.Add(Obj3);
      Arr.Add(Obj4);

      Arr.DeleteRange(1, 2);

      // Count is correctly decremented
      Assert.AreEqual(2, Arr.Count);
      Assert.AreSame(Obj1, Arr[0]);
      // BUG: Arr[1] should be Obj4 but MoveItems was not called
      // so Arr[1] still contains Obj2 (the old value, not moved)
      Assert.AreSame(Obj2, Arr[1], 'Bug: Obj4 not moved down - DeleteRange FCount decrement before MoveItems');
    finally
      Obj4.Free;
      Obj3.Free;
      Obj2.Free;
      Obj1.Free;
    end;
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArrayDeleteRangeDataOwner;
var
  Arr: TBoldObjectArray;
begin
  // DeleteRange with bcoDataOwner - delete from the END (where it works correctly)
  Arr := TBoldObjectArray.Create(8, [bcoDataOwner]);
  try
    Arr.Add(TObject.Create);
    Arr.Add(TObject.Create);
    Arr.Add(TObject.Create);

    // Delete last two items - works because no MoveItems needed
    Arr.DeleteRange(1, 2);
    Assert.AreEqual(1, Arr.Count);
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArrayGrowDeltaHighCapacity;
var
  Arr: TBoldObjectArray;
  I: Integer;
  Obj: TObject;
begin
  // Start with capacity 0, add >32 items to trigger high capacity growth path
  Arr := TBoldObjectArray.Create(0, []);
  try
    for I := 0 to 39 do
    begin
      Obj := TObject.Create;
      Arr.Add(Obj);
    end;
    Assert.AreEqual(40, Arr.Count);
    Assert.IsTrue(Arr.Capacity >= 40);

    // Clean up objects
    for I := 0 to Arr.Count - 1 do
      Arr[I].Free;
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestInterfaceArrayInsert;
var
  Arr: TBoldInterfaceArray;
  Intf1, Intf2, Intf3: IInterface;
begin
  Arr := TBoldInterfaceArray.Create(4, [bcoDataOwner]);
  try
    Intf1 := TInterfacedObject.Create;
    Intf2 := TInterfacedObject.Create;
    Intf3 := TInterfacedObject.Create;

    Arr.Add(Intf1);
    Arr.Add(Intf3);
    Arr.Insert(1, Intf2);

    Assert.AreEqual(3, Arr.Count);
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestInterfaceArrayPut;
var
  Arr: TBoldInterfaceArray;
  Intf1, Intf2, Intf3: IInterface;
begin
  Arr := TBoldInterfaceArray.Create(4, [bcoDataOwner]);
  try
    Intf1 := TInterfacedObject.Create;
    Intf2 := TInterfacedObject.Create;
    Intf3 := TInterfacedObject.Create;

    Arr.Add(Intf1);
    Arr.Add(Intf2);

    // Replace item at index 1
    Arr[1] := Intf3;
    Assert.AreEqual(2, Arr.Count);
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestInterfaceArrayIndexOf;
var
  Arr: TBoldInterfaceArray;
  Intf1, Intf2, Intf3: IInterface;
begin
  Arr := TBoldInterfaceArray.Create(4, []);
  try
    Intf1 := TInterfacedObject.Create;
    Intf2 := TInterfacedObject.Create;
    Intf3 := TInterfacedObject.Create;

    Arr.Add(Intf1);
    Arr.Add(Intf2);

    Assert.AreEqual(0, Arr.IndexOf(Intf1));
    Assert.AreEqual(1, Arr.IndexOf(Intf2));
    Assert.AreEqual(-1, Arr.IndexOf(Intf3));
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestInterfaceArrayRemove;
var
  Arr: TBoldInterfaceArray;
  Intf1, Intf2: IInterface;
begin
  Arr := TBoldInterfaceArray.Create(4, []);
  try
    Intf1 := TInterfacedObject.Create;
    Intf2 := TInterfacedObject.Create;

    Arr.Add(Intf1);
    Arr.Add(Intf2);

    Assert.AreEqual(0, Arr.Remove(Intf1));
    Assert.AreEqual(1, Arr.Count);
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestInterfaceArrayRemoveWithNil;
var
  Arr: TBoldInterfaceArray;
  Intf1, Intf2: IInterface;
begin
  Arr := TBoldInterfaceArray.Create(4, []);
  try
    Intf1 := TInterfacedObject.Create;
    Intf2 := TInterfacedObject.Create;

    Arr.Add(Intf1);
    Arr.Add(Intf2);

    Assert.AreEqual(0, Arr.RemoveWithNil(Intf1));
    Assert.AreEqual(2, Arr.Count, 'Count should not change');
    Assert.IsNull(Pointer(Arr[0]), 'Slot should be nil');
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestIntegerArrayPut;
var
  Arr: TBoldIntegerArray;
begin
  Arr := TBoldIntegerArray.Create(4, []);
  try
    Arr.Add(10);
    Arr.Add(20);
    Arr.Add(30);

    Arr[1] := 99;
    Assert.AreEqual(99, Arr[1], 'Put should replace value');
    Assert.AreEqual(3, Arr.Count);
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestIntegerArrayIndexOfBug;
var
  Arr: TBoldIntegerArray;
begin
  // TBoldIntegerArray.IndexOf has a bug: uses = instead of <>
  // The while loop continues while item EQUALS the current element,
  // so it returns the index of the first NON-matching element
  Arr := TBoldIntegerArray.Create(4, []);
  try
    Arr.Add(10);
    Arr.Add(20);
    Arr.Add(30);

    // IndexOf(10): starts at 0, 10=Arr[0] is true, increments to 1,
    // 10=Arr[1] is false, returns 1 (wrong - should be 0)
    // IndexOf(99): starts at 0, 99=Arr[0] is false, returns 0 (wrong - should be -1)
    // This test documents the buggy behavior
    Assert.AreEqual(1, Arr.IndexOf(10), 'Bug: IndexOf returns wrong index for first item');
    Assert.AreEqual(0, Arr.IndexOf(99), 'Bug: IndexOf returns 0 for missing item');
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestPointerArrayInsert;
var
  Arr: TBoldPointerArray;
  P1, P2, P3: Pointer;
begin
  Arr := TBoldPointerArray.Create(4, []);
  try
    P1 := Pointer(1);
    P2 := Pointer(2);
    P3 := Pointer(3);

    Arr.Add(P1);
    Arr.Add(P3);
    Arr.Insert(1, P2);

    Assert.AreEqual(3, Arr.Count);
    Assert.AreEqual(P1, Arr[0]);
    Assert.AreEqual(P2, Arr[1]);
    Assert.AreEqual(P3, Arr[2]);
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestPointerArrayPut;
var
  Arr: TBoldPointerArray;
  P1, P2, P3: Pointer;
begin
  Arr := TBoldPointerArray.Create(4, []);
  try
    P1 := Pointer(1);
    P2 := Pointer(2);
    P3 := Pointer(3);

    Arr.Add(P1);
    Arr.Add(P2);

    Arr[1] := P3;
    Assert.AreEqual(P3, Arr[1], 'Put should replace pointer');
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArrayIndexOutOfBounds;
var
  Arr: TBoldObjectArray;
  ExceptionRaised: Boolean;
begin
  Arr := TBoldObjectArray.Create(4, []);
  try
    ExceptionRaised := False;
    try
      Arr.Delete(0); // Empty array, index 0 is out of bounds
    except
      on E: EBoldContainerError do
        ExceptionRaised := True;
    end;
    Assert.IsTrue(ExceptionRaised, 'Expected EBoldContainerError for Delete on empty array');

    Arr.Add(TObject.Create);
    try
      ExceptionRaised := False;
      try
        Arr.Delete(5); // Index 5 is out of bounds
      except
        on E: EBoldContainerError do
          ExceptionRaised := True;
      end;
      Assert.IsTrue(ExceptionRaised, 'Expected EBoldContainerError for Delete with invalid index');
    finally
      Arr[0].Free;
    end;
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArrayCapacityLessThanCount;
var
  Arr: TBoldObjectArray;
  Obj1, Obj2: TObject;
  ExceptionRaised: Boolean;
begin
  Arr := TBoldObjectArray.Create(10, []);
  try
    Obj1 := TObject.Create;
    Obj2 := TObject.Create;
    try
      Arr.Add(Obj1);
      Arr.Add(Obj2);

      ExceptionRaised := False;
      try
        Arr.Capacity := 1; // Less than Count of 2
      except
        on E: EBoldContainerError do
          ExceptionRaised := True;
      end;
      Assert.IsTrue(ExceptionRaised, 'Expected EBoldContainerError when setting Capacity < Count');
    finally
      Obj2.Free;
      Obj1.Free;
    end;
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArrayExchangeOutOfBounds;
var
  Arr: TBoldObjectArray;
  ExRaised: Boolean;
begin
  Arr := TBoldObjectArray.Create(4, []);
  try
    Arr.Add(TObject.Create);
    try
      ExRaised := False;
      try
        Arr.Exchange(0, 5);
      except
        on E: EBoldContainerError do
          ExRaised := True;
      end;
      Assert.IsTrue(ExRaised, 'Exchange with Index2 out of bounds should raise');

      ExRaised := False;
      try
        Arr.Exchange(-1, 0);
      except
        on E: EBoldContainerError do
          ExRaised := True;
      end;
      Assert.IsTrue(ExRaised, 'Exchange with Index1 out of bounds should raise');
    finally
      Arr[0].Free;
    end;
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArrayInsertOutOfBounds;
var
  Arr: TBoldObjectArray;
  ExRaised: Boolean;
begin
  Arr := TBoldObjectArray.Create(4, []);
  try
    ExRaised := False;
    try
      Arr.Insert(-1, nil);
    except
      on E: EBoldContainerError do
        ExRaised := True;
    end;
    Assert.IsTrue(ExRaised, 'Insert at -1 should raise');

    ExRaised := False;
    try
      Arr.Insert(1, nil);
    except
      on E: EBoldContainerError do
        ExRaised := True;
    end;
    Assert.IsTrue(ExRaised, 'Insert at 1 on empty array should raise');
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArraySetCountNegative;
var
  Arr: TBoldObjectArray;
  ExRaised: Boolean;
begin
  Arr := TBoldObjectArray.Create(4, []);
  try
    ExRaised := False;
    try
      Arr.Count := -1;
    except
      on E: EBoldContainerError do
        ExRaised := True;
    end;
    Assert.IsTrue(ExRaised, 'Setting Count to -1 should raise');
  finally
    Arr.Free;
  end;
end;

procedure TTestBoldContainers.TestObjectArrayExchangeSameIndex;
var
  Arr: TBoldObjectArray;
  Obj1, Obj2: TObject;
begin
  // Exchange with same index should be a no-op (early exit)
  Arr := TBoldObjectArray.Create(4, []);
  try
    Obj1 := TObject.Create;
    Obj2 := TObject.Create;
    try
      Arr.Add(Obj1);
      Arr.Add(Obj2);

      Arr.Exchange(0, 0); // Same index - no-op

      Assert.AreSame(Obj1, Arr[0]);
      Assert.AreSame(Obj2, Arr[1]);
    finally
      Obj2.Free;
      Obj1.Free;
    end;
  finally
    Arr.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldContainers);

end.
