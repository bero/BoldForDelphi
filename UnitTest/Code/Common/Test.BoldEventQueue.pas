unit Test.BoldEventQueue;

interface

uses
  BoldTestFramework,
  BoldEventQueue;

type
  TTestBoldEventQueue = class(TBoldTestCase)
  private
    FQueue: TBoldEventQueue;
    FEventCalled: Boolean;
    FEventSender: TObject;
    FEventCallCount: Integer;
    procedure HandleEvent(Sender: TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // TBoldEventQueueItem tests
    procedure TestEventQueueItemCreate;
    procedure TestEventQueueItemSendEvent;

    // TBoldEventQueue tests
    procedure TestEventQueueCreate;
    procedure TestEventQueueAdd;
    procedure TestEventQueueDequeueOne;
    procedure TestEventQueueDequeueAll;
    procedure TestEventQueueRemoveAllForReceiver;
    procedure TestEventQueueRemoveAllForReceiver_EmptyQueue;
    procedure TestEventQueueCount;
  end;

implementation

uses
  SysUtils;

{ TTestBoldEventQueue }

procedure TTestBoldEventQueue.SetUp;
begin
  FQueue := TBoldEventQueue.Create;
  FEventCalled := False;
  FEventSender := nil;
  FEventCallCount := 0;
end;

procedure TTestBoldEventQueue.TearDown;
begin
  FQueue.Free;
end;

procedure TTestBoldEventQueue.HandleEvent(Sender: TObject);
begin
  FEventCalled := True;
  FEventSender := Sender;
  Inc(FEventCallCount);
end;

procedure TTestBoldEventQueue.TestEventQueueItemCreate;
var
  Item: TBoldEventQueueItem;
  TestReceiver: TObject;
begin
  TestReceiver := TObject.Create;
  try
    Item := TBoldEventQueueItem.Create(HandleEvent, Self, TestReceiver);
    try
      Assert.IsNotNull(Item);
      Assert.AreEqual(TObject(Self), Item.Sender);
      Assert.AreEqual(TestReceiver, Item.Receiver);
    finally
      Item.Free;
    end;
  finally
    TestReceiver.Free;
  end;
end;

procedure TTestBoldEventQueue.TestEventQueueItemSendEvent;
var
  Item: TBoldEventQueueItem;
begin
  Item := TBoldEventQueueItem.Create(HandleEvent, Self, nil);
  try
    Item.SendEvent;
    Assert.IsTrue(FEventCalled);
    Assert.AreEqual(TObject(Self), FEventSender);
  finally
    Item.Free;
  end;
end;

procedure TTestBoldEventQueue.TestEventQueueCreate;
begin
  Assert.IsNotNull(FQueue);
  Assert.AreEqual(0, FQueue.Count);
end;

procedure TTestBoldEventQueue.TestEventQueueAdd;
begin
  FQueue.Add(HandleEvent, Self, nil);
  Assert.AreEqual(1, FQueue.Count);
  FQueue.Add(HandleEvent, Self, nil);
  Assert.AreEqual(2, FQueue.Count);
end;

procedure TTestBoldEventQueue.TestEventQueueDequeueOne;
begin
  FQueue.Add(HandleEvent, Self, nil);
  Assert.AreEqual(1, FQueue.Count);
  FQueue.DequeueOne;
  Assert.AreEqual(0, FQueue.Count);
  Assert.IsTrue(FEventCalled);
end;

procedure TTestBoldEventQueue.TestEventQueueDequeueAll;
begin
  FQueue.Add(HandleEvent, Self, nil);
  FQueue.Add(HandleEvent, Self, nil);
  FQueue.Add(HandleEvent, Self, nil);
  Assert.AreEqual(3, FQueue.Count);
  FQueue.DequeueAll;
  Assert.AreEqual(0, FQueue.Count);
  Assert.AreEqual(3, FEventCallCount);
end;

procedure TTestBoldEventQueue.TestEventQueueRemoveAllForReceiver;
var
  Receiver1, Receiver2: TObject;
begin
  Receiver1 := TObject.Create;
  Receiver2 := TObject.Create;
  try
    FQueue.Add(HandleEvent, Self, Receiver1);
    FQueue.Add(HandleEvent, Self, Receiver2);
    FQueue.Add(HandleEvent, Self, Receiver1);
    Assert.AreEqual(3, FQueue.Count);

    FQueue.RemoveAllForReceiver(Receiver1);
    Assert.AreEqual(1, FQueue.Count);
    // Event should not be called - items are just removed
    Assert.AreEqual(0, FEventCallCount);
  finally
    Receiver1.Free;
    Receiver2.Free;
  end;
end;

procedure TTestBoldEventQueue.TestEventQueueRemoveAllForReceiver_EmptyQueue;
var
  Receiver: TObject;
begin
  Receiver := TObject.Create;
  try
    // Should not raise on empty queue
    FQueue.RemoveAllForReceiver(Receiver);
    Assert.AreEqual(0, FQueue.Count);
  finally
    Receiver.Free;
  end;
end;

procedure TTestBoldEventQueue.TestEventQueueCount;
begin
  Assert.AreEqual(0, FQueue.Count);
  FQueue.Add(HandleEvent, Self, nil);
  Assert.AreEqual(1, FQueue.Count);
end;

initialization
  BoldRegisterTest(TTestBoldEventQueue);

end.
