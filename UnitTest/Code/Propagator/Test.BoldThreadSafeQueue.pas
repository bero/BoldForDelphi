unit Test.BoldThreadSafeQueue;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.SyncObjs,
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
    [Test]
    procedure TestEnqueueDequeue;
    [Test]
    procedure TestQueueNotEmptyEvent;
    [Test]
    procedure TestMultipleEnqueue;
  end;

implementation

uses
  System.SysUtils,
  Winapi.Windows;

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

initialization
  TDUnitX.RegisterTestFixture(TTestBoldThreadSafeQueue);

end.
