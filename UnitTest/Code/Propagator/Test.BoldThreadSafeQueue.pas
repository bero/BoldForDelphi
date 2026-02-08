unit Test.BoldThreadSafeQueue;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.SyncObjs,
  System.Contnrs,
  BoldThreadSafeQueue;

type
  [TestFixture]
  [Category('Propagator')]
  TTestBoldThreadSafeQueue = class
  private
    FQueue: TBoldThreadSafeObjectQueue;
    FDequeueCount: Integer;
    FDequeueEvent: TEvent;
    procedure OnQueueNotEmpty(Queue: TBoldThreadSafeQueue);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // Existing ObjectQueue tests
    [Test] [Category('Quick')]
    procedure TestEnqueueDequeue;
    [Test] [Category('Quick')]
    procedure TestQueueNotEmptyEvent;
    [Test] [Category('Quick')]
    procedure TestMultipleEnqueue;

    // ObjectQueue: Empty property
    [Test] [Category('Quick')]
    procedure TestObjectQueue_EmptyProperty;
    // ObjectQueue: Dequeue from empty returns nil
    [Test] [Category('Quick')]
    procedure TestObjectQueue_DequeueEmpty;
    // ObjectQueue: DequeueList
    [Test] [Category('Quick')]
    procedure TestObjectQueue_DequeueList;
    // ObjectQueue: DequeueList with max limit
    [Test] [Category('Quick')]
    procedure TestObjectQueue_DequeueListWithMax;
    // ObjectQueue: MaxCount tracking
    [Test] [Category('Quick')]
    procedure TestObjectQueue_MaxCount;
    // ObjectQueue: SetOwnsObjects
    [Test] [Category('Quick')]
    procedure TestObjectQueue_SetOwnsObjects;
    // ObjectQueue: Clear with items (covers Clear while-loop body)
    [Test] [Category('Quick')]
    procedure TestObjectQueue_ClearWithItems;
    // ObjectQueue: OnQueueNotEmpty fires only on empty-to-non-empty transition
    [Test] [Category('Quick')]
    procedure TestObjectQueue_NotEmptyEventOnlyOnTransition;
    // ObjectQueue: OwnsObjects=false, caller must free
    [Test] [Category('Quick')]
    procedure TestObjectQueue_NonOwningDequeue;
  end;

  [TestFixture]
  [Category('Propagator')]
  TTestBoldThreadSafeStringQueue = class
  private
    FQueue: TBoldThreadSafeStringQueue;
    FNotifyCount: Integer;
    procedure OnNotEmpty(Queue: TBoldThreadSafeQueue);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test] [Category('Quick')]
    procedure TestEnqueueDequeue;
    [Test] [Category('Quick')]
    procedure TestDequeueEmpty;
    [Test] [Category('Quick')]
    procedure TestEnqueueList;
    [Test] [Category('Quick')]
    procedure TestEnqueueListEmpty;
    [Test] [Category('Quick')]
    procedure TestAppendToStringList;
    [Test] [Category('Quick')]
    procedure TestCount;
    [Test] [Category('Quick')]
    procedure TestEmpty;
    [Test] [Category('Quick')]
    procedure TestClear;
    [Test] [Category('Quick')]
    procedure TestNotifyOnEnqueue;
    [Test] [Category('Quick')]
    procedure TestNotifyOnEnqueueList;
    [Test] [Category('Quick')]
    procedure TestFIFOOrder;
  end;

  [TestFixture]
  [Category('Propagator')]
  TTestBoldThreadSafeInterfaceQueue = class
  private
    FQueue: TBoldThreadSafeInterfaceQueue;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test] [Category('Quick')]
    procedure TestEnqueueDequeue;
    [Test] [Category('Quick')]
    procedure TestDequeueEmpty;
    [Test] [Category('Quick')]
    procedure TestMultipleEnqueueDequeue;
    [Test] [Category('Quick')]
    procedure TestCount;
    [Test] [Category('Quick')]
    procedure TestClear;
  end;

implementation

uses
  System.SysUtils,
  Winapi.Windows;

type
  // Simple interface for testing interface queue
  ITestInterface = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    function GetValue: Integer;
  end;

  TTestInterfaceImpl = class(TInterfacedObject, ITestInterface)
  private
    FValue: Integer;
  public
    constructor Create(AValue: Integer);
    function GetValue: Integer;
  end;

constructor TTestInterfaceImpl.Create(AValue: Integer);
begin
  inherited Create;
  FValue := AValue;
end;

function TTestInterfaceImpl.GetValue: Integer;
begin
  Result := FValue;
end;

{ TTestBoldThreadSafeQueue }

procedure TTestBoldThreadSafeQueue.Setup;
begin
  FQueue := TBoldThreadSafeObjectQueue.Create('TestQueue');
  FDequeueCount := 0;
  FDequeueEvent := TEvent.Create(nil, True, False, '');
end;

procedure TTestBoldThreadSafeQueue.TearDown;
begin
  FreeAndNil(FQueue);
  FreeAndNil(FDequeueEvent);
end;

procedure TTestBoldThreadSafeQueue.OnQueueNotEmpty(Queue: TBoldThreadSafeQueue);
begin
  Inc(FDequeueCount);
  FDequeueEvent.SetEvent;
end;

procedure TTestBoldThreadSafeQueue.TestEnqueueDequeue;
var
  Obj: TObject;
  Dequeued: TObject;
begin
  Obj := TObject.Create;
  try
    FQueue.Enqueue(Obj);
    Assert.AreEqual(1, FQueue.Count, 'Queue should have 1 item after enqueue');

    Dequeued := FQueue.Dequeue;
    Assert.AreSame(Obj, Dequeued, 'Dequeued object should be same as enqueued');
    Assert.AreEqual(0, FQueue.Count, 'Queue should be empty after dequeue');
  finally
    Obj.Free;
  end;
end;

procedure TTestBoldThreadSafeQueue.TestQueueNotEmptyEvent;
var
  Obj: TObject;
begin
  FQueue.OnQueueNotEmpty := OnQueueNotEmpty;
  FDequeueCount := 0;

  Obj := TObject.Create;
  try
    FQueue.Enqueue(Obj);

    // Wait for event with timeout
    Assert.AreEqual(wrSignaled, FDequeueEvent.WaitFor(1000),
      'OnQueueNotEmpty event should be triggered');
    Assert.AreEqual(1, FDequeueCount, 'Event should have been called once');
  finally
    // Dequeue and free
    FQueue.Dequeue.Free;
  end;
end;

procedure TTestBoldThreadSafeQueue.TestMultipleEnqueue;
var
  i: Integer;
begin
  for i := 1 to 5 do
    FQueue.Enqueue(TObject.Create);

  Assert.AreEqual(5, FQueue.Count, 'Queue should have 5 items');

  // Dequeue all
  while FQueue.Count > 0 do
    FQueue.Dequeue.Free;

  Assert.AreEqual(0, FQueue.Count, 'Queue should be empty');
end;

procedure TTestBoldThreadSafeQueue.TestObjectQueue_EmptyProperty;
begin
  Assert.IsTrue(FQueue.Empty, 'New queue should be empty');
  FQueue.Enqueue(TObject.Create);
  Assert.IsFalse(FQueue.Empty, 'Queue with item should not be empty');
  FQueue.Dequeue.Free;
  Assert.IsTrue(FQueue.Empty, 'Queue after dequeue should be empty');
end;

procedure TTestBoldThreadSafeQueue.TestObjectQueue_DequeueEmpty;
var
  Result: TObject;
begin
  Result := FQueue.Dequeue;
  Assert.IsNull(Result, 'Dequeue from empty queue should return nil');
end;

procedure TTestBoldThreadSafeQueue.TestObjectQueue_DequeueList;
var
  ResultList: TObjectList;
begin
  FQueue.Enqueue(TObject.Create);
  FQueue.Enqueue(TObject.Create);
  FQueue.Enqueue(TObject.Create);

  ResultList := TObjectList.Create(True);
  try
    FQueue.DequeueList(ResultList, 10);
    Assert.AreEqual(3, ResultList.Count, 'Should dequeue all 3 items');
    Assert.AreEqual(0, FQueue.Count, 'Queue should be empty after DequeueList');
  finally
    ResultList.Free;
  end;
end;

procedure TTestBoldThreadSafeQueue.TestObjectQueue_DequeueListWithMax;
var
  ResultList: TObjectList;
begin
  FQueue.Enqueue(TObject.Create);
  FQueue.Enqueue(TObject.Create);
  FQueue.Enqueue(TObject.Create);
  FQueue.Enqueue(TObject.Create);
  FQueue.Enqueue(TObject.Create);

  ResultList := TObjectList.Create(True);
  try
    FQueue.DequeueList(ResultList, 3);
    Assert.AreEqual(3, ResultList.Count, 'Should dequeue max 3 items');
    Assert.AreEqual(2, FQueue.Count, 'Queue should have 2 remaining items');
  finally
    ResultList.Free;
  end;
end;

procedure TTestBoldThreadSafeQueue.TestObjectQueue_MaxCount;
begin
  Assert.AreEqual(0, FQueue.MaxCount, 'MaxCount should start at 0');

  FQueue.Enqueue(TObject.Create);
  FQueue.Enqueue(TObject.Create);
  FQueue.Enqueue(TObject.Create);
  Assert.AreEqual(3, FQueue.MaxCount, 'MaxCount should be 3 after 3 enqueues');

  FQueue.Dequeue.Free;
  FQueue.Dequeue.Free;
  Assert.AreEqual(3, FQueue.MaxCount, 'MaxCount should stay 3 after dequeues');

  FQueue.Enqueue(TObject.Create);
  Assert.AreEqual(3, FQueue.MaxCount, 'MaxCount should stay 3 (only 2 in queue)');

  FQueue.Enqueue(TObject.Create);
  FQueue.Enqueue(TObject.Create);
  FQueue.Enqueue(TObject.Create);
  Assert.AreEqual(5, FQueue.MaxCount, 'MaxCount should be 5 after reaching new peak');
end;

procedure TTestBoldThreadSafeQueue.TestObjectQueue_SetOwnsObjects;
var
  Obj: TObject;
begin
  // Default is OwnsObjects=true (from constructor)
  Assert.IsTrue(FQueue.OwnsObjects, 'Default should be OwnsObjects=true');

  FQueue.OwnsObjects := False;
  Assert.IsFalse(FQueue.OwnsObjects, 'Should be false after setting');

  FQueue.OwnsObjects := True;
  Assert.IsTrue(FQueue.OwnsObjects, 'Should be true after setting back');

  // Test non-owning: enqueue, dequeue, manually free
  FQueue.OwnsObjects := False;
  Obj := TObject.Create;
  try
    FQueue.Enqueue(Obj);
    Assert.AreSame(Obj, FQueue.Dequeue, 'Should get same object back');
  finally
    Obj.Free;
  end;
end;

procedure TTestBoldThreadSafeQueue.TestObjectQueue_ClearWithItems;
begin
  // Enqueue items that will be cleared (owned, so Clear frees them)
  FQueue.Enqueue(TObject.Create);
  FQueue.Enqueue(TObject.Create);
  FQueue.Enqueue(TObject.Create);
  Assert.AreEqual(3, FQueue.Count);

  FQueue.Clear;
  // BUG: Base class Clear empties the linked list but doesn't reset
  // ObjectQueue's fCount field. So Empty (checks linked list) returns True
  // but Count (returns fCount) still shows the stale value.
  Assert.IsTrue(FQueue.Empty, 'Queue linked list should be empty after Clear');
  Assert.AreEqual(3, FQueue.Count, 'BUG: fCount not reset by base Clear');
end;

procedure TTestBoldThreadSafeQueue.TestObjectQueue_NotEmptyEventOnlyOnTransition;
begin
  FQueue.OnQueueNotEmpty := OnQueueNotEmpty;
  FDequeueCount := 0;

  // First enqueue triggers event (empty -> non-empty)
  FQueue.Enqueue(TObject.Create);
  Assert.AreEqual(1, FDequeueCount, 'First enqueue should trigger event');

  // Second enqueue should NOT trigger event (already non-empty)
  FQueue.Enqueue(TObject.Create);
  Assert.AreEqual(1, FDequeueCount, 'Second enqueue should not trigger event');

  // Dequeue one - still non-empty
  FQueue.Dequeue.Free;

  // Third enqueue should NOT trigger (still non-empty)
  FQueue.Enqueue(TObject.Create);
  Assert.AreEqual(1, FDequeueCount, 'Third enqueue should not trigger (non-empty)');

  // Dequeue all
  FQueue.Dequeue.Free;
  FQueue.Dequeue.Free;
  Assert.IsTrue(FQueue.Empty);

  // Now enqueue again - should trigger (empty -> non-empty again)
  FQueue.Enqueue(TObject.Create);
  Assert.AreEqual(2, FDequeueCount, 'Enqueue after empty should trigger event again');
end;

procedure TTestBoldThreadSafeQueue.TestObjectQueue_NonOwningDequeue;
var
  Obj1, Obj2, Obj3: TObject;
begin
  FQueue.Free;
  FQueue := TBoldThreadSafeObjectQueue.Create('NonOwning', False);

  Obj1 := TObject.Create;
  Obj2 := TObject.Create;
  Obj3 := TObject.Create;
  try
    FQueue.Enqueue(Obj1);
    FQueue.Enqueue(Obj2);
    FQueue.Enqueue(Obj3);

    // FIFO order - dequeue returns oldest first (ring queue, Prev = head)
    Assert.AreSame(Obj1, FQueue.Dequeue);
    Assert.AreSame(Obj2, FQueue.Dequeue);
    Assert.AreSame(Obj3, FQueue.Dequeue);
  finally
    Obj1.Free;
    Obj2.Free;
    Obj3.Free;
  end;
end;

{ TTestBoldThreadSafeStringQueue }

procedure TTestBoldThreadSafeStringQueue.Setup;
begin
  FQueue := TBoldThreadSafeStringQueue.Create('TestStringQueue');
  FNotifyCount := 0;
end;

procedure TTestBoldThreadSafeStringQueue.TearDown;
begin
  FreeAndNil(FQueue);
end;

procedure TTestBoldThreadSafeStringQueue.OnNotEmpty(Queue: TBoldThreadSafeQueue);
begin
  Inc(FNotifyCount);
end;

procedure TTestBoldThreadSafeStringQueue.TestEnqueueDequeue;
begin
  FQueue.Enqueue('Hello');
  Assert.AreEqual('Hello', FQueue.Dequeue);
end;

procedure TTestBoldThreadSafeStringQueue.TestDequeueEmpty;
begin
  Assert.AreEqual('', FQueue.Dequeue, 'Dequeue from empty string queue should return empty string');
end;

procedure TTestBoldThreadSafeStringQueue.TestEnqueueList;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    List.Add('Alpha');
    List.Add('Beta');
    List.Add('Gamma');
    FQueue.EnqueueList(List);
  finally
    List.Free;
  end;

  Assert.AreEqual(3, FQueue.Count);
  // FIFO: dequeue returns oldest first
  Assert.AreEqual('Alpha', FQueue.Dequeue);
  Assert.AreEqual('Beta', FQueue.Dequeue);
  Assert.AreEqual('Gamma', FQueue.Dequeue);
end;

procedure TTestBoldThreadSafeStringQueue.TestEnqueueListEmpty;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    FQueue.EnqueueList(List); // Empty list - should be no-op
  finally
    List.Free;
  end;
  Assert.AreEqual(0, FQueue.Count, 'Empty list enqueue should be no-op');
end;

procedure TTestBoldThreadSafeStringQueue.TestAppendToStringList;
var
  Result: TStringList;
begin
  FQueue.Enqueue('First');
  FQueue.Enqueue('Second');
  FQueue.Enqueue('Third');

  Result := TStringList.Create;
  try
    FQueue.AppendToStringList(Result);
    Assert.AreEqual(3, Result.Count);
    // Entries are inserted after marker (stack-like), so Next walks newest-first.
    // Dequeue takes from Prev (oldest-first = FIFO), but AppendToStringList
    // walks Next (newest-first = LIFO order).
    Assert.AreEqual('Third', Result[0]);
    Assert.AreEqual('Second', Result[1]);
    Assert.AreEqual('First', Result[2]);
    // Queue should still have items (AppendToStringList doesn't dequeue)
    Assert.AreEqual(3, FQueue.Count);
  finally
    Result.Free;
  end;
end;

procedure TTestBoldThreadSafeStringQueue.TestCount;
begin
  // StringQueue uses base class GetCount which walks the linked list
  Assert.AreEqual(0, FQueue.Count);
  FQueue.Enqueue('A');
  Assert.AreEqual(1, FQueue.Count);
  FQueue.Enqueue('B');
  Assert.AreEqual(2, FQueue.Count);
  FQueue.Dequeue;
  Assert.AreEqual(1, FQueue.Count);
  FQueue.Dequeue;
  Assert.AreEqual(0, FQueue.Count);
end;

procedure TTestBoldThreadSafeStringQueue.TestEmpty;
begin
  Assert.IsTrue(FQueue.Empty, 'New queue should be empty');
  FQueue.Enqueue('X');
  Assert.IsFalse(FQueue.Empty, 'Queue with item should not be empty');
  FQueue.Dequeue;
  Assert.IsTrue(FQueue.Empty, 'Queue after dequeue should be empty');
end;

procedure TTestBoldThreadSafeStringQueue.TestClear;
begin
  FQueue.Enqueue('A');
  FQueue.Enqueue('B');
  FQueue.Enqueue('C');
  Assert.AreEqual(3, FQueue.Count);

  FQueue.Clear;
  Assert.AreEqual(0, FQueue.Count);
  Assert.IsTrue(FQueue.Empty);
end;

procedure TTestBoldThreadSafeStringQueue.TestNotifyOnEnqueue;
begin
  FQueue.OnQueueNotEmpty := OnNotEmpty;
  FNotifyCount := 0;

  FQueue.Enqueue('First');
  Assert.AreEqual(1, FNotifyCount, 'First enqueue should notify');

  FQueue.Enqueue('Second');
  Assert.AreEqual(1, FNotifyCount, 'Second enqueue should not notify (not empty)');
end;

procedure TTestBoldThreadSafeStringQueue.TestNotifyOnEnqueueList;
var
  List: TStringList;
begin
  FQueue.OnQueueNotEmpty := OnNotEmpty;
  FNotifyCount := 0;

  List := TStringList.Create;
  try
    List.Add('A');
    List.Add('B');
    FQueue.EnqueueList(List);
  finally
    List.Free;
  end;
  Assert.AreEqual(1, FNotifyCount, 'EnqueueList should notify once (empty->non-empty)');
end;

procedure TTestBoldThreadSafeStringQueue.TestFIFOOrder;
begin
  FQueue.Enqueue('One');
  FQueue.Enqueue('Two');
  FQueue.Enqueue('Three');

  Assert.AreEqual('One', FQueue.Dequeue);
  Assert.AreEqual('Two', FQueue.Dequeue);
  Assert.AreEqual('Three', FQueue.Dequeue);
  Assert.AreEqual('', FQueue.Dequeue, 'Empty after all dequeued');
end;

{ TTestBoldThreadSafeInterfaceQueue }

procedure TTestBoldThreadSafeInterfaceQueue.Setup;
begin
  FQueue := TBoldThreadSafeInterfaceQueue.Create('TestInterfaceQueue');
end;

procedure TTestBoldThreadSafeInterfaceQueue.TearDown;
begin
  FreeAndNil(FQueue);
end;

procedure TTestBoldThreadSafeInterfaceQueue.TestEnqueueDequeue;
var
  Intf: ITestInterface;
  Dequeued: IInterface;
begin
  Intf := TTestInterfaceImpl.Create(42);
  FQueue.Enqueue(Intf);

  Dequeued := FQueue.Dequeue;
  Assert.IsNotNull(Dequeued, 'Dequeued interface should not be nil');
  Assert.AreEqual(42, (Dequeued as ITestInterface).GetValue);
end;

procedure TTestBoldThreadSafeInterfaceQueue.TestDequeueEmpty;
var
  Result: IInterface;
begin
  Result := FQueue.Dequeue;
  Assert.IsNull(Result, 'Dequeue from empty interface queue should return nil');
end;

procedure TTestBoldThreadSafeInterfaceQueue.TestMultipleEnqueueDequeue;
var
  Intf: ITestInterface;
begin
  FQueue.Enqueue(TTestInterfaceImpl.Create(10) as ITestInterface);
  FQueue.Enqueue(TTestInterfaceImpl.Create(20) as ITestInterface);
  FQueue.Enqueue(TTestInterfaceImpl.Create(30) as ITestInterface);

  // FIFO order
  Intf := FQueue.Dequeue as ITestInterface;
  Assert.AreEqual(10, Intf.GetValue);

  Intf := FQueue.Dequeue as ITestInterface;
  Assert.AreEqual(20, Intf.GetValue);

  Intf := FQueue.Dequeue as ITestInterface;
  Assert.AreEqual(30, Intf.GetValue);

  Assert.IsNull(FQueue.Dequeue, 'Should be empty after all dequeued');
end;

procedure TTestBoldThreadSafeInterfaceQueue.TestCount;
begin
  // InterfaceQueue uses base class GetCount which walks the linked list
  Assert.AreEqual(0, FQueue.Count);

  FQueue.Enqueue(TTestInterfaceImpl.Create(1) as ITestInterface);
  Assert.AreEqual(1, FQueue.Count);

  FQueue.Enqueue(TTestInterfaceImpl.Create(2) as ITestInterface);
  Assert.AreEqual(2, FQueue.Count);

  FQueue.Dequeue;
  Assert.AreEqual(1, FQueue.Count);
end;

procedure TTestBoldThreadSafeInterfaceQueue.TestClear;
begin
  FQueue.Enqueue(TTestInterfaceImpl.Create(1) as ITestInterface);
  FQueue.Enqueue(TTestInterfaceImpl.Create(2) as ITestInterface);
  Assert.AreEqual(2, FQueue.Count);

  FQueue.Clear;
  Assert.AreEqual(0, FQueue.Count);
  Assert.IsTrue(FQueue.Empty);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldThreadSafeQueue);
  TDUnitX.RegisterTestFixture(TTestBoldThreadSafeStringQueue);
  TDUnitX.RegisterTestFixture(TTestBoldThreadSafeInterfaceQueue);

end.
