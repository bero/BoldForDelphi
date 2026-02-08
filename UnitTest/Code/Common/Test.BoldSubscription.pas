unit Test.BoldSubscription;

interface

uses
  DUnitX.TestFramework,
  BoldDefs,
  BoldSubscription,
  BoldSubscribableCollection;

type
  [TestFixture]
  TTestBoldSubscription = class
  private
    FReceivedOriginators: array of TObject;
    FReceivedOriginalEvents: array of TBoldEvent;
    FReceivedRequestedEvents: array of TBoldRequestedEvent;
    FReceiveCallCount: Integer;
    FReceiveCallCount2: Integer;

    FExtendedCallCount: Integer;
    FExtendedArgCount: Integer;

    FAnswerResult: Boolean;
    FAnswerCallCount: Integer;

    FDelayedActionCallCount: Integer;
    FDestroyEventReceived: Boolean;

    procedure HandleReceive(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent);
    procedure HandleReceive2(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent);
    procedure HandleReceiveCheckDestroying(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent);
    procedure HandleReceiveAndQueueDelayed(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent);
    procedure HandleExtendedReceive(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent; const Args: array of const);
    function HandleAnswer(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent; const Args: array of const;
      Subscriber: TBoldSubscriber): Boolean;
    procedure HandleDelayedAction(Sender: TObject);

    procedure RecordEvent(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    { Core Pub/Sub: TBoldPublisher + TBoldPassthroughSubscriber }
    [Test]
    procedure TestSubscribeAndReceiveSmallEvent;
    [Test]
    procedure TestSubscribeSmallEventSet;
    [Test]
    procedure TestNonMatchingEventNotReceived;
    [Test]
    procedure TestLargeEventSubscription;
    [Test]
    procedure TestSmallEventDeduplication;
    [Test]
    procedure TestMultipleSubscribersOnOnePublisher;
    [Test]
    procedure TestOneSubscriberMultiplePublishers;
    [Test]
    procedure TestCancelAllSubscriptions;
    [Test]
    procedure TestCancelSubscriptionTo;
    [Test]
    procedure TestHasMatchingSubscription;
    [Test]
    procedure TestDestroyingSubscriberClearsPublisherSide;
    [Test]
    procedure TestNotifySubscribersAndClearSubscriptions;
    [Test]
    procedure TestRequestedEventRemapping;

    { Extended Events and Queries }
    [Test]
    procedure TestExtendedPassthroughReceivesArgs;
    [Test]
    procedure TestQueryVetoPattern;
    [Test]
    procedure TestQueryApprovePattern;
    [Test]
    procedure TestQueryNoHandler;

    { TBoldSubscribableObject }
    [Test]
    procedure TestSubscribableObject_LazyPublisher;
    [Test]
    procedure TestSubscribableObject_SendEventNoSubscribers;
    [Test]
    procedure TestSubscribableObject_SendEventWithSubscribers;
    [Test]
    procedure TestSubscribableObject_SendQueryNoSubscribers;
    [Test]
    procedure TestSubscribableObject_DestroyNotifiesSubscribers;

    { TBoldSubscribableCollection }
    [Test]
    procedure TestSubscribableCollection_EagerPublisher;
    [Test]
    procedure TestSubscribableCollection_SubscribeAndReceive;

    { TBoldSubscribableComponent }
    [Test]
    procedure TestSubscribableComponent_LazyPublisher;
    [Test]
    procedure TestSubscribableComponent_SendEventWithSubscribers;
    [Test]
    procedure TestSubscribableComponent_DestroyNotifiesSubscribers;

    { TBoldSubscribablePersistent }
    [Test]
    procedure TestSubscribablePersistent_SubscribeAndReceive;
    [Test]
    procedure TestSubscribablePersistent_DestroyNotifiesSubscribers;

    { TBoldSubscribableNonRefCountedObject }
    [Test]
    procedure TestSubscribableNonRefCounted_SubscribeAndReceive;
    [Test]
    procedure TestSubscribableNonRefCounted_DestroyNotifiesSubscribers;

    { Edge cases }
    [Test]
    procedure TestAddSmallSubscription_EmptySetRaisesException;
    [Test]
    procedure TestAddSubscription_BigEventDeduplication;
    [Test]
    procedure TestSubscriber_CancelSubscriptionTo;
    [Test]
    procedure TestSendExtendedEvent_WithArgs;
    [Test]
    procedure TestSendQuery_WithOriginator;
    [Test]
    procedure TestSubscriptionsAsText;
    [Test]
    procedure TestContextString_Publisher;

    { Statistics }
    [Test]
    procedure TestGlobalStatistics;

    { Post-Notification Queue }
    [Test]
    procedure TestDelayTillAfterNotification;
  end;

implementation

uses
  SysUtils,
  Classes;

{ TTestBoldSubscription }

procedure TTestBoldSubscription.Setup;
begin
  FReceiveCallCount := 0;
  FReceiveCallCount2 := 0;
  FExtendedCallCount := 0;
  FExtendedArgCount := 0;
  FAnswerResult := True;
  FAnswerCallCount := 0;
  FDelayedActionCallCount := 0;
  FDestroyEventReceived := False;
  SetLength(FReceivedOriginators, 0);
  SetLength(FReceivedOriginalEvents, 0);
  SetLength(FReceivedRequestedEvents, 0);
end;

procedure TTestBoldSubscription.TearDown;
begin
  SetLength(FReceivedOriginators, 0);
  SetLength(FReceivedOriginalEvents, 0);
  SetLength(FReceivedRequestedEvents, 0);
end;

procedure TTestBoldSubscription.RecordEvent(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
var
  Len: Integer;
begin
  Len := Length(FReceivedOriginators);
  SetLength(FReceivedOriginators, Len + 1);
  SetLength(FReceivedOriginalEvents, Len + 1);
  SetLength(FReceivedRequestedEvents, Len + 1);
  FReceivedOriginators[Len] := Originator;
  FReceivedOriginalEvents[Len] := OriginalEvent;
  FReceivedRequestedEvents[Len] := RequestedEvent;
end;

procedure TTestBoldSubscription.HandleReceive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  Inc(FReceiveCallCount);
  RecordEvent(Originator, OriginalEvent, RequestedEvent);
end;

procedure TTestBoldSubscription.HandleReceive2(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  Inc(FReceiveCallCount2);
end;

procedure TTestBoldSubscription.HandleReceiveCheckDestroying(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if OriginalEvent = beDestroying then
    FDestroyEventReceived := True;
end;

procedure TTestBoldSubscription.HandleReceiveAndQueueDelayed(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  Inc(FReceiveCallCount);
  BoldAddEventToPostNotifyQueue(HandleDelayedAction, nil, Self);
end;

procedure TTestBoldSubscription.HandleExtendedReceive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  Inc(FExtendedCallCount);
  FExtendedArgCount := Length(Args);
  RecordEvent(Originator, OriginalEvent, RequestedEvent);
end;

function TTestBoldSubscription.HandleAnswer(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
begin
  Inc(FAnswerCallCount);
  Result := FAnswerResult;
end;

procedure TTestBoldSubscription.HandleDelayedAction(Sender: TObject);
begin
  Inc(FDelayedActionCallCount);
end;

{ Core Pub/Sub Tests }

procedure TTestBoldSubscription.TestSubscribeAndReceiveSmallEvent;
var
  PublisherVar: TBoldPublisher;
  Subscriber: TBoldPassthroughSubscriber;
begin
  PublisherVar := nil;
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    PublisherVar := TBoldPublisher.Create(PublisherVar);
    try
      PublisherVar.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);
      PublisherVar.SendEvent(beValueChanged);

      Assert.AreEqual(1, FReceiveCallCount, 'Should receive exactly one event');
      Assert.AreEqual(Integer(beValueChanged), Integer(FReceivedOriginalEvents[0]), 'OriginalEvent should be beValueChanged');
    finally
      PublisherVar.NotifySubscribersAndClearSubscriptions(nil);
      PublisherVar.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscribeSmallEventSet;
var
  PublisherVar: TBoldPublisher;
  Subscriber: TBoldPassthroughSubscriber;
begin
  PublisherVar := nil;
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    PublisherVar := TBoldPublisher.Create(PublisherVar);
    try
      PublisherVar.AddSmallSubscription(Subscriber, [beItemAdded, beItemDeleted, beValueChanged], beDefaultRequestedEvent);

      PublisherVar.SendEvent(beItemAdded);
      PublisherVar.SendEvent(beItemDeleted);
      PublisherVar.SendEvent(beValueChanged);

      Assert.AreEqual(3, FReceiveCallCount, 'Should receive all three matching events');
      Assert.AreEqual(Integer(beItemAdded), Integer(FReceivedOriginalEvents[0]));
      Assert.AreEqual(Integer(beItemDeleted), Integer(FReceivedOriginalEvents[1]));
      Assert.AreEqual(Integer(beValueChanged), Integer(FReceivedOriginalEvents[2]));
    finally
      PublisherVar.NotifySubscribersAndClearSubscriptions(nil);
      PublisherVar.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestNonMatchingEventNotReceived;
var
  PublisherVar: TBoldPublisher;
  Subscriber: TBoldPassthroughSubscriber;
begin
  PublisherVar := nil;
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    PublisherVar := TBoldPublisher.Create(PublisherVar);
    try
      PublisherVar.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);
      PublisherVar.SendEvent(beItemAdded);

      Assert.AreEqual(0, FReceiveCallCount, 'Non-matching event should not be received');
    finally
      PublisherVar.NotifySubscribersAndClearSubscriptions(nil);
      PublisherVar.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestLargeEventSubscription;
var
  PublisherVar: TBoldPublisher;
  Subscriber: TBoldPassthroughSubscriber;
  LargeEvent: TBoldEvent;
begin
  PublisherVar := nil;
  LargeEvent := boeClassChanged; // 52, a big event (>29)
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    PublisherVar := TBoldPublisher.Create(PublisherVar);
    try
      PublisherVar.AddSubscription(Subscriber, LargeEvent, beDefaultRequestedEvent);
      PublisherVar.SendEvent(LargeEvent);

      Assert.AreEqual(1, FReceiveCallCount, 'Large event should be received');
      Assert.AreEqual(Integer(LargeEvent), Integer(FReceivedOriginalEvents[0]));
    finally
      PublisherVar.NotifySubscribersAndClearSubscriptions(nil);
      PublisherVar.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestSmallEventDeduplication;
var
  PublisherVar: TBoldPublisher;
  Subscriber: TBoldPassthroughSubscriber;
begin
  PublisherVar := nil;
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    PublisherVar := TBoldPublisher.Create(PublisherVar);
    try
      // Subscribe to [beValueChanged] with same requestedEvent
      PublisherVar.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);
      // Subscribe again with [beItemAdded] — should extend the existing subscription, not create new
      PublisherVar.AddSmallSubscription(Subscriber, [beItemAdded], beDefaultRequestedEvent);

      Assert.AreEqual(1, PublisherVar.SubscriptionCount, 'Deduplication should merge into one subscription');
      Assert.AreEqual(1, Subscriber.SubscriptionCount, 'Subscriber should have one subscription reference');

      // Both events should be received
      PublisherVar.SendEvent(beValueChanged);
      PublisherVar.SendEvent(beItemAdded);
      Assert.AreEqual(2, FReceiveCallCount, 'Both events from merged subscription should be received');
    finally
      PublisherVar.NotifySubscribersAndClearSubscriptions(nil);
      PublisherVar.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestMultipleSubscribersOnOnePublisher;
var
  PublisherVar: TBoldPublisher;
  Sub1, Sub2: TBoldPassthroughSubscriber;
begin
  PublisherVar := nil;
  Sub1 := TBoldPassthroughSubscriber.Create(HandleReceive);
  Sub2 := TBoldPassthroughSubscriber.Create(HandleReceive2);
  try
    PublisherVar := TBoldPublisher.Create(PublisherVar);
    try
      PublisherVar.AddSmallSubscription(Sub1, [beValueChanged], beDefaultRequestedEvent);
      PublisherVar.AddSmallSubscription(Sub2, [beValueChanged], beDefaultRequestedEvent);

      PublisherVar.SendEvent(beValueChanged);

      Assert.AreEqual(1, FReceiveCallCount, 'Subscriber 1 should receive event');
      Assert.AreEqual(1, FReceiveCallCount2, 'Subscriber 2 should receive event');
    finally
      PublisherVar.NotifySubscribersAndClearSubscriptions(nil);
      PublisherVar.Free;
    end;
  finally
    Sub1.Free;
    Sub2.Free;
  end;
end;

procedure TTestBoldSubscription.TestOneSubscriberMultiplePublishers;
var
  Pub1Var, Pub2Var: TBoldPublisher;
  Subscriber: TBoldPassthroughSubscriber;
begin
  Pub1Var := nil;
  Pub2Var := nil;
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    Pub1Var := TBoldPublisher.Create(Pub1Var);
    try
      Pub2Var := TBoldPublisher.Create(Pub2Var);
      try
        Pub1Var.AddSmallSubscription(Subscriber, [beValueChanged], 100);
        Pub2Var.AddSmallSubscription(Subscriber, [beItemAdded], 200);

        Assert.AreEqual(2, Subscriber.SubscriptionCount, 'Subscriber should track two publishers');

        Pub1Var.SendEvent(beValueChanged);
        Pub2Var.SendEvent(beItemAdded);

        Assert.AreEqual(2, FReceiveCallCount, 'Should receive from both publishers');
        Assert.AreEqual(100, FReceivedRequestedEvents[0], 'First event should have RequestedEvent 100');
        Assert.AreEqual(200, FReceivedRequestedEvents[1], 'Second event should have RequestedEvent 200');
      finally
        Pub2Var.NotifySubscribersAndClearSubscriptions(nil);
        Pub2Var.Free;
      end;
    finally
      Pub1Var.NotifySubscribersAndClearSubscriptions(nil);
      Pub1Var.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestCancelAllSubscriptions;
var
  PublisherVar: TBoldPublisher;
  Subscriber: TBoldPassthroughSubscriber;
begin
  PublisherVar := nil;
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    PublisherVar := TBoldPublisher.Create(PublisherVar);
    try
      PublisherVar.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);
      Assert.IsTrue(PublisherVar.HasSubscribers, 'Publisher should have subscribers');

      Subscriber.CancelAllSubscriptions;

      Assert.AreEqual(0, Subscriber.SubscriptionCount, 'Subscriber should have no subscriptions after cancel');
      PublisherVar.SendEvent(beValueChanged);
      Assert.AreEqual(0, FReceiveCallCount, 'Should not receive events after CancelAllSubscriptions');
    finally
      PublisherVar.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestCancelSubscriptionTo;
var
  Pub1Var, Pub2Var: TBoldPublisher;
  Subscriber: TBoldPassthroughSubscriber;
begin
  Pub1Var := nil;
  Pub2Var := nil;
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    Pub1Var := TBoldPublisher.Create(Pub1Var);
    try
      Pub2Var := TBoldPublisher.Create(Pub2Var);
      try
        Pub1Var.AddSmallSubscription(Subscriber, [beValueChanged], 100);
        Pub2Var.AddSmallSubscription(Subscriber, [beItemAdded], 200);

        // Cancel only Pub1
        Pub1Var.CancelSubscriptionTo(Subscriber);

        Pub1Var.SendEvent(beValueChanged);
        Assert.AreEqual(0, FReceiveCallCount, 'Should not receive from cancelled publisher');

        Pub2Var.SendEvent(beItemAdded);
        Assert.AreEqual(1, FReceiveCallCount, 'Should still receive from active publisher');
        Assert.AreEqual(200, FReceivedRequestedEvents[0], 'Should be from Pub2');
      finally
        Pub2Var.NotifySubscribersAndClearSubscriptions(nil);
        Pub2Var.Free;
      end;
    finally
      Pub1Var.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestHasMatchingSubscription;
var
  PublisherVar: TBoldPublisher;
  Sub1, Sub2: TBoldPassthroughSubscriber;
begin
  PublisherVar := nil;
  Sub1 := TBoldPassthroughSubscriber.Create(HandleReceive);
  Sub2 := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    PublisherVar := TBoldPublisher.Create(PublisherVar);
    try
      PublisherVar.AddSmallSubscription(Sub1, [beValueChanged], beDefaultRequestedEvent);

      Assert.IsTrue(PublisherVar.HasMatchingSubscription(Sub1), 'Sub1 should have matching subscription');
      Assert.IsFalse(PublisherVar.HasMatchingSubscription(Sub2), 'Sub2 should not have matching subscription');

      // Also test from subscriber side
      Assert.IsTrue(Sub1.HasMatchingSubscription(PublisherVar), 'Sub1.HasMatchingSubscription should return True');
      Assert.IsFalse(Sub2.HasMatchingSubscription(PublisherVar), 'Sub2.HasMatchingSubscription should return False');
    finally
      PublisherVar.NotifySubscribersAndClearSubscriptions(nil);
      PublisherVar.Free;
    end;
  finally
    Sub1.Free;
    Sub2.Free;
  end;
end;

procedure TTestBoldSubscription.TestDestroyingSubscriberClearsPublisherSide;
var
  PublisherVar: TBoldPublisher;
  Subscriber: TBoldPassthroughSubscriber;
begin
  PublisherVar := nil;
  PublisherVar := TBoldPublisher.Create(PublisherVar);
  try
    Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
    PublisherVar.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);
    Assert.IsTrue(PublisherVar.HasSubscribers, 'Publisher should have subscribers before destroy');

    // Destroying subscriber should clean up publisher's subscription entries
    Subscriber.Free;

    // Publisher should no longer deliver events (subscription slot cleared)
    PublisherVar.SendEvent(beValueChanged);
    Assert.AreEqual(0, FReceiveCallCount, 'No events should be delivered after subscriber is destroyed');
  finally
    PublisherVar.Free;
  end;
end;

procedure TTestBoldSubscription.TestNotifySubscribersAndClearSubscriptions;
var
  PublisherVar: TBoldPublisher;
  Subscriber: TBoldPassthroughSubscriber;
begin
  PublisherVar := nil;
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceiveCheckDestroying);
  try
    PublisherVar := TBoldPublisher.Create(PublisherVar);
    try
      PublisherVar.AddSmallSubscription(Subscriber, [beDestroying], beDefaultRequestedEvent);

      PublisherVar.NotifySubscribersAndClearSubscriptions(nil);

      Assert.IsTrue(FDestroyEventReceived, 'beDestroying event should be sent');
      Assert.AreEqual(0, PublisherVar.SubscriptionCount, 'All subscriptions should be cleared');
    finally
      PublisherVar.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestRequestedEventRemapping;
var
  PublisherVar: TBoldPublisher;
  Subscriber: TBoldPassthroughSubscriber;
  CustomRequestedEvent: TBoldRequestedEvent;
begin
  PublisherVar := nil;
  CustomRequestedEvent := 42;
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    PublisherVar := TBoldPublisher.Create(PublisherVar);
    try
      PublisherVar.AddSmallSubscription(Subscriber, [beValueChanged], CustomRequestedEvent);
      PublisherVar.SendEvent(beValueChanged);

      Assert.AreEqual(1, FReceiveCallCount);
      Assert.AreEqual(Integer(beValueChanged), Integer(FReceivedOriginalEvents[0]), 'OriginalEvent preserved');
      Assert.AreEqual(Integer(CustomRequestedEvent), FReceivedRequestedEvents[0], 'RequestedEvent should be remapped to 42');
    finally
      PublisherVar.NotifySubscribersAndClearSubscriptions(nil);
      PublisherVar.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

{ Extended Events and Queries }

procedure TTestBoldSubscription.TestExtendedPassthroughReceivesArgs;
var
  PublisherVar: TBoldPublisher;
  Subscriber: TBoldExtendedPassthroughSubscriber;
begin
  PublisherVar := nil;
  Subscriber := TBoldExtendedPassthroughSubscriber.CreateWithExtendedReceive(HandleExtendedReceive);
  try
    PublisherVar := TBoldPublisher.Create(PublisherVar);
    try
      PublisherVar.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);
      PublisherVar.SendExtendedEvent(nil, beValueChanged, [42, 'hello']);

      Assert.AreEqual(1, FExtendedCallCount, 'Extended receive should be called once');
      Assert.AreEqual(2, FExtendedArgCount, 'Should receive 2 args');
    finally
      PublisherVar.NotifySubscribersAndClearSubscriptions(nil);
      PublisherVar.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestQueryVetoPattern;
var
  PublisherVar: TBoldPublisher;
  Subscriber: TBoldExtendedPassthroughSubscriber;
  QueryResult: Boolean;
begin
  PublisherVar := nil;
  FAnswerResult := False; // Subscriber will veto
  Subscriber := TBoldExtendedPassthroughSubscriber.CreateWithReceiveAndAnswer(HandleReceive, HandleAnswer);
  try
    PublisherVar := TBoldPublisher.Create(PublisherVar);
    try
      PublisherVar.AddSubscription(Subscriber, bqMayModify, bqMayModify);
      QueryResult := PublisherVar.SendQuery(nil, bqMayModify, [], nil);

      Assert.IsFalse(QueryResult, 'Query should be vetoed when subscriber returns False');
      Assert.AreEqual(1, FAnswerCallCount, 'Answer should be called once');
    finally
      PublisherVar.NotifySubscribersAndClearSubscriptions(nil);
      PublisherVar.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestQueryApprovePattern;
var
  PublisherVar: TBoldPublisher;
  Subscriber: TBoldExtendedPassthroughSubscriber;
  QueryResult: Boolean;
begin
  PublisherVar := nil;
  FAnswerResult := True; // Subscriber will approve
  Subscriber := TBoldExtendedPassthroughSubscriber.CreateWithReceiveAndAnswer(HandleReceive, HandleAnswer);
  try
    PublisherVar := TBoldPublisher.Create(PublisherVar);
    try
      PublisherVar.AddSubscription(Subscriber, bqMayModify, bqMayModify);
      QueryResult := PublisherVar.SendQuery(nil, bqMayModify, [], nil);

      Assert.IsTrue(QueryResult, 'Query should be approved when subscriber returns True');
      Assert.AreEqual(1, FAnswerCallCount, 'Answer should be called once');
    finally
      PublisherVar.NotifySubscribersAndClearSubscriptions(nil);
      PublisherVar.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestQueryNoHandler;
var
  PublisherVar: TBoldPublisher;
  Subscriber: TBoldExtendedPassthroughSubscriber;
  QueryResult: Boolean;
begin
  PublisherVar := nil;
  // CreateWithReceiveAndAnswer with nil answer func — Answer returns True by default
  Subscriber := TBoldExtendedPassthroughSubscriber.CreateWithReceiveAndAnswer(nil, nil);
  try
    PublisherVar := TBoldPublisher.Create(PublisherVar);
    try
      PublisherVar.AddSubscription(Subscriber, bqMayModify, bqMayModify);
      QueryResult := PublisherVar.SendQuery(nil, bqMayModify, [], nil);

      Assert.IsTrue(QueryResult, 'Query should return True when no answer handler is assigned');
    finally
      PublisherVar.NotifySubscribersAndClearSubscriptions(nil);
      PublisherVar.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

{ TBoldSubscribableObject tests }

procedure TTestBoldSubscription.TestSubscribableObject_LazyPublisher;
var
  Obj: TBoldSubscribableObject;
begin
  Obj := TBoldSubscribableObject.Create;
  try
    // Before any subscription, HasSubscribers should be False (no publisher created)
    Assert.IsFalse(Obj.HasSubscribers, 'Should have no subscribers initially');
  finally
    Obj.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscribableObject_SendEventNoSubscribers;
var
  Obj: TBoldSubscribableObject;
begin
  Obj := TBoldSubscribableObject.Create;
  try
    // SendEvent with no publisher should be a no-op (no crash)
    Obj.SendEvent(beValueChanged);
    Assert.Pass('SendEvent with no subscribers should not crash');
  finally
    Obj.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscribableObject_SendEventWithSubscribers;
var
  Obj: TBoldSubscribableObject;
  Subscriber: TBoldPassthroughSubscriber;
begin
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    Obj := TBoldSubscribableObject.Create;
    try
      Obj.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);
      Assert.IsTrue(Obj.HasSubscribers, 'Should have subscribers after AddSmallSubscription');

      Obj.SendEvent(beValueChanged);
      Assert.AreEqual(1, FReceiveCallCount, 'Subscriber should receive the event');
    finally
      Obj.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscribableObject_SendQueryNoSubscribers;
var
  Obj: TBoldSubscribableObject;
  QueryResult: Boolean;
begin
  Obj := TBoldSubscribableObject.Create;
  try
    QueryResult := Obj.SendQuery(bqMayModify, [], nil);
    Assert.IsTrue(QueryResult, 'SendQuery with no subscribers should return True (approved)');
  finally
    Obj.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscribableObject_DestroyNotifiesSubscribers;
var
  Obj: TBoldSubscribableObject;
  Subscriber: TBoldPassthroughSubscriber;
begin
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceiveCheckDestroying);
  try
    Obj := TBoldSubscribableObject.Create;
    Obj.AddSmallSubscription(Subscriber, [beDestroying], beDefaultRequestedEvent);

    Obj.Free; // Should send beDestroying

    Assert.IsTrue(FDestroyEventReceived, 'Destroying subscribable object should send beDestroying');
    Assert.AreEqual(0, Subscriber.SubscriptionCount, 'Subscriber should have no subscriptions after object destroyed');
  finally
    Subscriber.Free;
  end;
end;

{ TBoldSubscribableCollection tests }

procedure TTestBoldSubscription.TestSubscribableCollection_EagerPublisher;
var
  Coll: TBoldSubscribableCollection;
begin
  Coll := TBoldSubscribableCollection.Create(TCollectionItem);
  try
    // Eager publisher: created in constructor, but no subscribers yet
    Assert.IsFalse(Coll.HasSubscribers, 'No subscribers initially');
  finally
    Coll.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscribableCollection_SubscribeAndReceive;
var
  Coll: TBoldSubscribableCollection;
  Subscriber: TBoldPassthroughSubscriber;
begin
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    Coll := TBoldSubscribableCollection.Create(TCollectionItem);
    try
      Coll.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);
      Assert.IsTrue(Coll.HasSubscribers, 'Collection should have subscribers');

      Coll.SendEvent(beValueChanged);
      Assert.AreEqual(1, FReceiveCallCount, 'Subscriber should receive event from collection');
    finally
      Coll.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

{ TBoldSubscribableComponent tests }

procedure TTestBoldSubscription.TestSubscribableComponent_LazyPublisher;
var
  Comp: TBoldSubscribableComponent;
begin
  Comp := TBoldSubscribableComponent.Create(nil);
  try
    Assert.IsFalse(Comp.HasSubscribers, 'Should have no subscribers initially');
  finally
    Comp.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscribableComponent_SendEventWithSubscribers;
var
  Comp: TBoldSubscribableComponent;
  Subscriber: TBoldPassthroughSubscriber;
begin
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    Comp := TBoldSubscribableComponent.Create(nil);
    try
      Comp.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);
      Assert.IsTrue(Comp.HasSubscribers, 'Should have subscribers after AddSmallSubscription');

      Comp.SendEvent(Comp, beValueChanged);
      Assert.AreEqual(1, FReceiveCallCount, 'Subscriber should receive the event');
      Assert.AreSame(Comp, FReceivedOriginators[0], 'Originator should be the component');
    finally
      Comp.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscribableComponent_DestroyNotifiesSubscribers;
var
  Comp: TBoldSubscribableComponent;
  Subscriber: TBoldPassthroughSubscriber;
begin
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceiveCheckDestroying);
  try
    Comp := TBoldSubscribableComponent.Create(nil);
    Comp.AddSmallSubscription(Subscriber, [beDestroying], beDefaultRequestedEvent);

    Comp.Free;

    Assert.IsTrue(FDestroyEventReceived, 'Destroying component should send beDestroying');
    Assert.AreEqual(0, Subscriber.SubscriptionCount, 'Subscriber should have no subscriptions after component destroyed');
  finally
    Subscriber.Free;
  end;
end;

{ TBoldSubscribablePersistent tests }

procedure TTestBoldSubscription.TestSubscribablePersistent_SubscribeAndReceive;
var
  Pers: TBoldSubscribablePersistent;
  Subscriber: TBoldPassthroughSubscriber;
begin
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    Pers := TBoldSubscribablePersistent.Create;
    try
      Pers.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);
      Assert.IsTrue(Pers.HasSubscribers, 'Should have subscribers after AddSmallSubscription');

      Pers.SendEvent(Pers, beValueChanged);
      Assert.AreEqual(1, FReceiveCallCount, 'Subscriber should receive the event');
      Assert.AreSame(Pers, FReceivedOriginators[0], 'Originator should be the persistent object');
    finally
      Pers.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscribablePersistent_DestroyNotifiesSubscribers;
var
  Pers: TBoldSubscribablePersistent;
  Subscriber: TBoldPassthroughSubscriber;
begin
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceiveCheckDestroying);
  try
    Pers := TBoldSubscribablePersistent.Create;
    Pers.AddSmallSubscription(Subscriber, [beDestroying], beDefaultRequestedEvent);

    Pers.Free;

    Assert.IsTrue(FDestroyEventReceived, 'Destroying persistent should send beDestroying');
    Assert.AreEqual(0, Subscriber.SubscriptionCount, 'Subscriber should have no subscriptions after persistent destroyed');
  finally
    Subscriber.Free;
  end;
end;

{ TBoldSubscribableNonRefCountedObject tests }

procedure TTestBoldSubscription.TestSubscribableNonRefCounted_SubscribeAndReceive;
var
  Obj: TBoldSubscribableNonRefCountedObject;
  Subscriber: TBoldPassthroughSubscriber;
begin
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    Obj := TBoldSubscribableNonRefCountedObject.Create;
    try
      Obj.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);
      Assert.IsTrue(Obj.HasSubscribers, 'Should have subscribers after AddSmallSubscription');

      Obj.SendEvent(Obj, beValueChanged);
      Assert.AreEqual(1, FReceiveCallCount, 'Subscriber should receive the event');
      Assert.AreSame(Obj, FReceivedOriginators[0], 'Originator should be the object');
    finally
      Obj.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscribableNonRefCounted_DestroyNotifiesSubscribers;
var
  Obj: TBoldSubscribableNonRefCountedObject;
  Subscriber: TBoldPassthroughSubscriber;
begin
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceiveCheckDestroying);
  try
    Obj := TBoldSubscribableNonRefCountedObject.Create;
    Obj.AddSmallSubscription(Subscriber, [beDestroying], beDefaultRequestedEvent);

    Obj.Free;

    Assert.IsTrue(FDestroyEventReceived, 'Destroying non-ref-counted object should send beDestroying');
    Assert.AreEqual(0, Subscriber.SubscriptionCount, 'Subscriber should have no subscriptions after object destroyed');
  finally
    Subscriber.Free;
  end;
end;

{ Edge case tests }

procedure TTestBoldSubscription.TestAddSmallSubscription_EmptySetRaisesException;
var
  PublisherVar: TBoldPublisher;
  Subscriber: TBoldPassthroughSubscriber;
begin
  PublisherVar := nil;
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    PublisherVar := TBoldPublisher.Create(PublisherVar);
    try
      Assert.WillRaise(
        procedure
        begin
          PublisherVar.AddSmallSubscription(Subscriber, [], beDefaultRequestedEvent);
        end,
        EBold,
        'Empty event set should raise EBold');
    finally
      PublisherVar.NotifySubscribersAndClearSubscriptions(nil);
      PublisherVar.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestAddSubscription_BigEventDeduplication;
var
  PublisherVar: TBoldPublisher;
  Subscriber: TBoldPassthroughSubscriber;
begin
  PublisherVar := nil;
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    PublisherVar := TBoldPublisher.Create(PublisherVar);
    try
      // Subscribe to same big event twice with same RequestedEvent — should deduplicate
      PublisherVar.AddSubscription(Subscriber, boeClassChanged, beDefaultRequestedEvent);
      PublisherVar.AddSubscription(Subscriber, boeClassChanged, beDefaultRequestedEvent);

      Assert.AreEqual(1, PublisherVar.SubscriptionCount, 'Big event deduplication should merge into one subscription');

      PublisherVar.SendExtendedEvent(nil, boeClassChanged, []);
      Assert.AreEqual(1, FReceiveCallCount, 'Should receive exactly one event');
    finally
      PublisherVar.NotifySubscribersAndClearSubscriptions(nil);
      PublisherVar.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscriber_CancelSubscriptionTo;
var
  Pub1Var, Pub2Var: TBoldPublisher;
  Subscriber: TBoldPassthroughSubscriber;
begin
  Pub1Var := nil;
  Pub2Var := nil;
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    Pub1Var := TBoldPublisher.Create(Pub1Var);
    try
      Pub2Var := TBoldPublisher.Create(Pub2Var);
      try
        Pub1Var.AddSmallSubscription(Subscriber, [beValueChanged], 100);
        Pub2Var.AddSmallSubscription(Subscriber, [beItemAdded], 200);

        // Cancel via subscriber side (calls through to publisher)
        Subscriber.CancelSubscriptionTo(Pub1Var);

        Pub1Var.SendEvent(beValueChanged);
        Assert.AreEqual(0, FReceiveCallCount, 'Should not receive from cancelled publisher');

        Pub2Var.SendEvent(beItemAdded);
        Assert.AreEqual(1, FReceiveCallCount, 'Should still receive from active publisher');
      finally
        Pub2Var.NotifySubscribersAndClearSubscriptions(nil);
        Pub2Var.Free;
      end;
    finally
      Pub1Var.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestSendExtendedEvent_WithArgs;
var
  Obj: TBoldSubscribableObject;
  Subscriber: TBoldExtendedPassthroughSubscriber;
begin
  Subscriber := TBoldExtendedPassthroughSubscriber.CreateWithExtendedReceive(HandleExtendedReceive);
  try
    Obj := TBoldSubscribableObject.Create;
    try
      Obj.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);
      Obj.SendExtendedEvent(beValueChanged, [42, 'test']);

      Assert.AreEqual(1, FExtendedCallCount, 'Extended receive should be called');
      Assert.AreEqual(2, FExtendedArgCount, 'Should receive 2 args');
      Assert.AreSame(Obj, FReceivedOriginators[0], 'Originator should be the subscribable object');
    finally
      Obj.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestSendQuery_WithOriginator;
var
  Obj: TBoldSubscribableObject;
  ExternalOriginator: TObject;
  Subscriber: TBoldExtendedPassthroughSubscriber;
  QueryResult: Boolean;
begin
  FAnswerResult := True;
  ExternalOriginator := TObject.Create;
  try
    Subscriber := TBoldExtendedPassthroughSubscriber.CreateWithReceiveAndAnswer(HandleReceive, HandleAnswer);
    try
      Obj := TBoldSubscribableObject.Create;
      try
        Obj.AddSubscription(Subscriber, bqMayModify, bqMayModify);

        // SendQuery with explicit Originator param — should use that instead of Self
        QueryResult := Obj.SendQuery(bqMayModify, [], nil, ExternalOriginator);

        Assert.IsTrue(QueryResult, 'Query should be approved');
        Assert.AreEqual(1, FAnswerCallCount, 'Answer should be called once');
      finally
        Obj.Free;
      end;
    finally
      Subscriber.Free;
    end;
  finally
    ExternalOriginator.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscriptionsAsText;
var
  PublisherVar: TBoldPublisher;
  Subscriber: TBoldPassthroughSubscriber;
  Text: string;
begin
  PublisherVar := nil;
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    PublisherVar := TBoldPublisher.Create(PublisherVar);
    try
      PublisherVar.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);

      Text := PublisherVar.SubscriptionsAsText;
      Assert.IsNotEmpty(Text, 'SubscriptionsAsText should return non-empty string for active subscriptions');
      Assert.Contains(Text, '0:', 'Should contain subscription index');
    finally
      PublisherVar.NotifySubscribersAndClearSubscriptions(nil);
      PublisherVar.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestContextString_Publisher;
var
  PublisherVar: TBoldPublisher;
  Obj: TBoldSubscribableObject;
  ContextStr: string;
begin
  // Publisher without subscribable object
  PublisherVar := nil;
  PublisherVar := TBoldPublisher.Create(PublisherVar);
  try
    ContextStr := PublisherVar.ContextString;
    Assert.IsNotEmpty(ContextStr, 'ContextString should not be empty');
  finally
    PublisherVar.Free;
  end;

  // Publisher with subscribable object (via SubscribableObject)
  Obj := TBoldSubscribableObject.Create;
  try
    ContextStr := Obj.ContextString;
    Assert.AreEqual('TBoldSubscribableObject', ContextStr, 'ContextString should be the class name');
  finally
    Obj.Free;
  end;
end;

{ Statistics }

procedure TTestBoldSubscription.TestGlobalStatistics;
var
  PublisherVar: TBoldPublisher;
  Subscriber: TBoldPassthroughSubscriber;
  InitialPubCount, InitialSubCount, InitialActiveCount: Integer;
begin
  InitialPubCount := PublisherCount;
  InitialSubCount := SubscriberCount;
  InitialActiveCount := ActiveSubscriptionCount;

  PublisherVar := nil;
  PublisherVar := TBoldPublisher.Create(PublisherVar);
  try
    Assert.AreEqual(InitialPubCount + 1, PublisherCount, 'PublisherCount should increment');

    Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
    try
      Assert.AreEqual(InitialSubCount + 1, SubscriberCount, 'SubscriberCount should increment');

      PublisherVar.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);
      Assert.AreEqual(InitialActiveCount + 1, ActiveSubscriptionCount, 'ActiveSubscriptionCount should increment');

      Subscriber.CancelAllSubscriptions;
      Assert.AreEqual(InitialActiveCount, ActiveSubscriptionCount, 'ActiveSubscriptionCount should decrement after cancel');
    finally
      Subscriber.Free;
    end;

    Assert.AreEqual(InitialSubCount, SubscriberCount, 'SubscriberCount should decrement after subscriber freed');
  finally
    PublisherVar.Free;
  end;

  Assert.AreEqual(InitialPubCount, PublisherCount, 'PublisherCount should decrement after publisher freed');
end;

{ Post-Notification Queue }

procedure TTestBoldSubscription.TestDelayTillAfterNotification;
var
  PublisherVar: TBoldPublisher;
  Subscriber: TBoldPassthroughSubscriber;
begin
  PublisherVar := nil;
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceiveAndQueueDelayed);
  try
    PublisherVar := TBoldPublisher.Create(PublisherVar);
    try
      PublisherVar.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);

      // SendEvent wraps in StartNotify/EndNotify
      PublisherVar.SendEvent(beValueChanged);

      // The delayed action should have been executed after EndNotify
      Assert.AreEqual(1, FReceiveCallCount, 'Subscriber should receive the event');
      Assert.AreEqual(1, FDelayedActionCallCount, 'Delayed action should execute after notification completes');
    finally
      PublisherVar.NotifySubscribersAndClearSubscriptions(nil);
      PublisherVar.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldSubscription);

end.
