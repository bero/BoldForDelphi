unit Test.BoldSubscription;

interface

uses
  DUnitX.TestFramework,
  BoldDefs,
  BoldBase,
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

    { Subscriber Diagnostic Methods }
    [Test]
    procedure TestSubscriber_ContextString;
    [Test]
    procedure TestSubscriber_DebugInfo;
    [Test]
    procedure TestSubscriber_SubscriptionsAsText;
    [Test]
    procedure TestSubscriber_SubscriptionsAsText_Component;
    [Test]
    procedure TestSubscriber_SubscriptionsAsText_NoSubscriptions;
    [Test]
    procedure TestSubscriber_Answer_RaisesEBold;

    { Variant SendQuery/SendExtendedEvent }
    [Test]
    procedure TestSubscribableComponent_SendQuery;
    [Test]
    procedure TestSubscribableComponent_SendExtendedEvent;
    [Test]
    procedure TestSubscribableComponent_SubscriptionsAsText;
    [Test]
    procedure TestSubscribablePersistent_SendQuery;
    [Test]
    procedure TestSubscribablePersistent_SendExtendedEvent;
    [Test]
    procedure TestSubscribablePersistent_AddSubscription_BigEvent;
    [Test]
    procedure TestSubscribablePersistent_SubscriptionsAsText;
    [Test]
    procedure TestSubscribablePersistent_SubscriptionsAsText_NoPublisher;
    [Test]
    procedure TestSubscribableNonRefCounted_AddSubscription_BigEvent;
    [Test]
    procedure TestSubscribableNonRefCounted_SendExtendedEvent;
    [Test]
    procedure TestSubscribableNonRefCounted_SendQuery;
    [Test]
    procedure TestSubscribableNonRefCounted_SendQuery_WithOriginator;
    [Test]
    procedure TestSubscribableNonRefCounted_SubscriptionsAsText;

    { Publisher Loop Path Coverage }
    [Test]
    procedure TestHasMatchingSubscription_PublisherSidePath;
    [Test]
    procedure TestCancelSubscriptionTo_SubscriberSidePath;

    { Statistics }
    [Test]
    procedure TestGlobalStatistics;

    { Post-Notification Queue }
    [Test]
    procedure TestDelayTillAfterNotification;
    [Test]
    procedure TestDelayTillAfterNotification_Immediate;
    [Test]
    procedure TestBoldForcedDequeuePostNotify;

    { Deduplication — Subscriber-Side Paths }
    [Test]
    procedure TestSmallEventDeduplication_SubscriberSidePath;
    [Test]
    procedure TestBigEventDeduplication_SubscriberSidePath;

    { Publisher Diagnostics }
    [Test]
    procedure TestPublisher_ContextString_WithSubscribableObject;

    { Subscriber Diagnostics — Additional Paths }
    [Test]
    procedure TestSubscriber_ContextString_TComponentOwner;
    [Test]
    procedure TestSubscriber_BaseContextString;
    [Test]
    procedure TestSubscriber_SubscriptionsAsText_PlainObject;

    { SubscribableObject Diagnostics }
    [Test]
    procedure TestSubscribableObject_SubscriptionsAsText;

    { Array Growth }
    [Test]
    procedure TestManySubscriptions_TriggersLargeGrowth;
  end;

implementation

uses
  SysUtils,
  Classes;

type
  // Helper to access protected Answer method for testing
  TBoldSubscriberAccess = class(TBoldSubscriber)
  public
    // Expose the protected Receive method (abstract in base, but we just need Answer)
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent); override;
    function CallAnswer(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent; const Args: array of const;
      Subscriber: TBoldSubscriber): Boolean;
  end;

  // Helper TComponent descendant with a Receive handler for testing subscriber ContextString
  TTestHelperComponent = class(TComponent)
  public
    ReceiveCallCount: Integer;
    procedure HandleReceive(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent);
  end;

procedure TBoldSubscriberAccess.Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  // No-op, needed to make the class concrete
end;

function TBoldSubscriberAccess.CallAnswer(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
begin
  Result := Answer(Originator, OriginalEvent, RequestedEvent, Args, Subscriber);
end;

{ TTestHelperComponent }

procedure TTestHelperComponent.HandleReceive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  Inc(ReceiveCallCount);
end;

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

{ Subscriber Diagnostic Methods }

procedure TTestBoldSubscription.TestSubscriber_ContextString;
var
  Subscriber: TBoldPassthroughSubscriber;
begin
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    // GetContextString on TBoldPassthroughSubscriber extracts the method's object
    // and returns its class name (since test fixture is not TComponent or TBoldMemoryManagedObject)
    Assert.Contains(Subscriber.ContextString, 'TTestBoldSubscription',
      'ContextString should contain the class name of the method owner');
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscriber_DebugInfo;
var
  Subscriber: TBoldPassthroughSubscriber;
begin
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    // DebugInfo delegates to ContextString
    Assert.IsNotEmpty(Subscriber.DebugInfo, 'DebugInfo should return non-empty string');
    Assert.AreEqual(Subscriber.ContextString, Subscriber.DebugInfo,
      'DebugInfo should equal ContextString');
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscriber_SubscriptionsAsText;
var
  Obj: TBoldSubscribableObject;
  Subscriber: TBoldPassthroughSubscriber;
  Text: string;
begin
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    Obj := TBoldSubscribableObject.Create;
    try
      Obj.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);

      Text := Subscriber.SubscriptionsAsText;
      Assert.IsNotEmpty(Text, 'SubscriptionsAsText should be non-empty when subscribed');
      // SubscribableObject is TBoldSubscribableObject (TBoldMemoryManagedObject descendant)
      // so it should use DebugInfo path
      Assert.Contains(Text, '0:', 'Should contain subscription index');
    finally
      Obj.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscriber_SubscriptionsAsText_Component;
var
  Comp: TBoldSubscribableComponent;
  Subscriber: TBoldPassthroughSubscriber;
  Text: string;
begin
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    Comp := TBoldSubscribableComponent.Create(nil);
    try
      Comp.Name := 'TestComponent';
      Comp.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);

      Text := Subscriber.SubscriptionsAsText;
      Assert.IsNotEmpty(Text, 'SubscriptionsAsText should be non-empty');
      Assert.Contains(Text, 'TestComponent', 'Should contain component Name');
    finally
      Comp.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscriber_SubscriptionsAsText_NoSubscriptions;
var
  Subscriber: TBoldPassthroughSubscriber;
begin
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    Assert.AreEqual('', Subscriber.SubscriptionsAsText,
      'SubscriptionsAsText should be empty when no subscriptions');
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscriber_Answer_RaisesEBold;
var
  Subscriber: TBoldSubscriberAccess;
  Originator: TObject;
begin
  // TBoldSubscriberAccess inherits the base TBoldSubscriber.Answer which raises EBold
  Subscriber := TBoldSubscriberAccess.Create;
  Originator := TObject.Create;
  try
    Assert.WillRaise(
      procedure
      begin
        Subscriber.CallAnswer(Originator, beValueChanged, beDefaultRequestedEvent, [], nil);
      end,
      EBold,
      'Base Answer should raise EBold');
  finally
    Originator.Free;
    Subscriber.Free;
  end;
end;

{ Variant SendQuery/SendExtendedEvent }

procedure TTestBoldSubscription.TestSubscribableComponent_SendQuery;
var
  Comp: TBoldSubscribableComponent;
  Subscriber: TBoldExtendedPassthroughSubscriber;
  QueryResult: Boolean;
begin
  FAnswerResult := True;
  Subscriber := TBoldExtendedPassthroughSubscriber.CreateWithReceiveAndAnswer(HandleReceive, HandleAnswer);
  try
    Comp := TBoldSubscribableComponent.Create(nil);
    try
      Comp.AddSubscription(Subscriber, bqMayModify, bqMayModify);
      QueryResult := Comp.SendQuery(Comp, bqMayModify, [], nil);

      Assert.IsTrue(QueryResult, 'Query should be approved');
      Assert.AreEqual(1, FAnswerCallCount, 'Answer should be called');
    finally
      Comp.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscribableComponent_SendExtendedEvent;
var
  Comp: TBoldSubscribableComponent;
  Subscriber: TBoldExtendedPassthroughSubscriber;
begin
  Subscriber := TBoldExtendedPassthroughSubscriber.CreateWithExtendedReceive(HandleExtendedReceive);
  try
    Comp := TBoldSubscribableComponent.Create(nil);
    try
      Comp.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);
      Comp.SendExtendedEvent(Comp, beValueChanged, [42, 'test']);

      Assert.AreEqual(1, FExtendedCallCount, 'Extended receive should be called');
      Assert.AreEqual(2, FExtendedArgCount, 'Should receive 2 args');
    finally
      Comp.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscribableComponent_SubscriptionsAsText;
var
  Comp: TBoldSubscribableComponent;
  Subscriber: TBoldPassthroughSubscriber;
  Text: string;
begin
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    Comp := TBoldSubscribableComponent.Create(nil);
    try
      Comp.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);
      Text := Comp.SubscriptionsAsText;
      Assert.IsNotEmpty(Text, 'SubscriptionsAsText should be non-empty');
      Assert.Contains(Text, '0:', 'Should contain subscription index');
    finally
      Comp.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscribablePersistent_SendQuery;
var
  Pers: TBoldSubscribablePersistent;
  Subscriber: TBoldExtendedPassthroughSubscriber;
  QueryResult: Boolean;
begin
  FAnswerResult := True;
  Subscriber := TBoldExtendedPassthroughSubscriber.CreateWithReceiveAndAnswer(HandleReceive, HandleAnswer);
  try
    Pers := TBoldSubscribablePersistent.Create;
    try
      Pers.AddSubscription(Subscriber, bqMayModify, bqMayModify);
      QueryResult := Pers.SendQuery(Pers, bqMayModify, [], nil);

      Assert.IsTrue(QueryResult, 'Query should be approved');
      Assert.AreEqual(1, FAnswerCallCount, 'Answer should be called');
    finally
      Pers.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscribablePersistent_SendExtendedEvent;
var
  Pers: TBoldSubscribablePersistent;
  Subscriber: TBoldExtendedPassthroughSubscriber;
begin
  Subscriber := TBoldExtendedPassthroughSubscriber.CreateWithExtendedReceive(HandleExtendedReceive);
  try
    Pers := TBoldSubscribablePersistent.Create;
    try
      Pers.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);
      Pers.SendExtendedEvent(Pers, beValueChanged, [42, 'test']);

      Assert.AreEqual(1, FExtendedCallCount, 'Extended receive should be called');
      Assert.AreEqual(2, FExtendedArgCount, 'Should receive 2 args');
    finally
      Pers.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscribablePersistent_AddSubscription_BigEvent;
var
  Pers: TBoldSubscribablePersistent;
  Subscriber: TBoldPassthroughSubscriber;
begin
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    Pers := TBoldSubscribablePersistent.Create;
    try
      Pers.AddSubscription(Subscriber, boeClassChanged, beDefaultRequestedEvent);
      Pers.SendExtendedEvent(Pers, boeClassChanged, []);

      Assert.AreEqual(1, FReceiveCallCount, 'Should receive the big event');
    finally
      Pers.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscribablePersistent_SubscriptionsAsText;
var
  Pers: TBoldSubscribablePersistent;
  Subscriber: TBoldPassthroughSubscriber;
  Text: string;
begin
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    Pers := TBoldSubscribablePersistent.Create;
    try
      Pers.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);
      Text := Pers.SubscriptionsAsText;
      Assert.IsNotEmpty(Text, 'SubscriptionsAsText should be non-empty with subscriptions');
    finally
      Pers.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscribablePersistent_SubscriptionsAsText_NoPublisher;
var
  Pers: TBoldSubscribablePersistent;
begin
  Pers := TBoldSubscribablePersistent.Create;
  try
    // No publisher allocated yet (lazy), should return empty
    Assert.AreEqual('', Pers.SubscriptionsAsText,
      'SubscriptionsAsText should be empty when no publisher exists');
  finally
    Pers.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscribableNonRefCounted_AddSubscription_BigEvent;
var
  Obj: TBoldSubscribableNonRefCountedObject;
  Subscriber: TBoldPassthroughSubscriber;
begin
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    Obj := TBoldSubscribableNonRefCountedObject.Create;
    try
      Obj.AddSubscription(Subscriber, boeClassChanged, beDefaultRequestedEvent);
      Obj.SendExtendedEvent(Obj, boeClassChanged, []);

      Assert.AreEqual(1, FReceiveCallCount, 'Should receive the big event');
    finally
      Obj.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscribableNonRefCounted_SendExtendedEvent;
var
  Obj: TBoldSubscribableNonRefCountedObject;
  Subscriber: TBoldExtendedPassthroughSubscriber;
begin
  Subscriber := TBoldExtendedPassthroughSubscriber.CreateWithExtendedReceive(HandleExtendedReceive);
  try
    Obj := TBoldSubscribableNonRefCountedObject.Create;
    try
      Obj.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);
      Obj.SendExtendedEvent(Obj, beValueChanged, [42, 'test']);

      Assert.AreEqual(1, FExtendedCallCount, 'Extended receive should be called');
      Assert.AreEqual(2, FExtendedArgCount, 'Should receive 2 args');
    finally
      Obj.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscribableNonRefCounted_SendQuery;
var
  Obj: TBoldSubscribableNonRefCountedObject;
  Subscriber: TBoldExtendedPassthroughSubscriber;
  QueryResult: Boolean;
begin
  FAnswerResult := True;
  Subscriber := TBoldExtendedPassthroughSubscriber.CreateWithReceiveAndAnswer(HandleReceive, HandleAnswer);
  try
    Obj := TBoldSubscribableNonRefCountedObject.Create;
    try
      Obj.AddSubscription(Subscriber, bqMayModify, bqMayModify);
      // Originator = nil triggers the Self branch (line 1429)
      QueryResult := Obj.SendQuery(nil, bqMayModify, [], nil);

      Assert.IsTrue(QueryResult, 'Query should be approved');
      Assert.AreEqual(1, FAnswerCallCount, 'Answer should be called');
    finally
      Obj.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscribableNonRefCounted_SendQuery_WithOriginator;
var
  Obj: TBoldSubscribableNonRefCountedObject;
  ExternalOriginator: TObject;
  Subscriber: TBoldExtendedPassthroughSubscriber;
  QueryResult: Boolean;
begin
  FAnswerResult := True;
  ExternalOriginator := TObject.Create;
  try
    Subscriber := TBoldExtendedPassthroughSubscriber.CreateWithReceiveAndAnswer(HandleReceive, HandleAnswer);
    try
      Obj := TBoldSubscribableNonRefCountedObject.Create;
      try
        Obj.AddSubscription(Subscriber, bqMayModify, bqMayModify);
        // Explicit Originator triggers line 1427
        QueryResult := Obj.SendQuery(ExternalOriginator, bqMayModify, [], nil);

        Assert.IsTrue(QueryResult, 'Query should be approved');
        Assert.AreEqual(1, FAnswerCallCount, 'Answer should be called');
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

procedure TTestBoldSubscription.TestSubscribableNonRefCounted_SubscriptionsAsText;
var
  Obj: TBoldSubscribableNonRefCountedObject;
  Subscriber: TBoldPassthroughSubscriber;
  Text: string;
begin
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    Obj := TBoldSubscribableNonRefCountedObject.Create;
    try
      Obj.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);
      Text := Obj.SubscriptionsAsText;
      Assert.IsNotEmpty(Text, 'SubscriptionsAsText should be non-empty');
    finally
      Obj.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

{ Publisher Loop Path Coverage }

procedure TTestBoldSubscription.TestHasMatchingSubscription_PublisherSidePath;
var
  Pub1Var, Pub2Var: TBoldPublisher;
  Subscriber: TBoldPassthroughSubscriber;
begin
  // To trigger the publisher-side loop (line 655-660) in HasMatchingSubscription,
  // we need: pub.SubscriptionCount < sub.SubscriptionCount
  // So subscriber has many publishers, but one publisher has only 1 subscription
  Pub1Var := nil;
  Pub2Var := nil;
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    Pub1Var := TBoldPublisher.Create(Pub1Var);
    try
      Pub2Var := TBoldPublisher.Create(Pub2Var);
      try
        // Subscribe to both publishers — subscriber.SubscriptionCount = 2
        Pub1Var.AddSmallSubscription(Subscriber, [beValueChanged], 100);
        Pub2Var.AddSmallSubscription(Subscriber, [beItemAdded], 200);

        // Pub1 has 1 subscription, subscriber has 2 → pub count < sub count → publisher-side loop
        Assert.IsTrue(Pub1Var.HasMatchingSubscription(Subscriber),
          'Should find subscription via publisher-side loop');
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

procedure TTestBoldSubscription.TestCancelSubscriptionTo_SubscriberSidePath;
var
  PublisherVar: TBoldPublisher;
  Sub1, Sub2, Sub3: TBoldPassthroughSubscriber;
begin
  // To trigger subscriber-side cancel loop (lines 685-687):
  // pub.SubscriptionCount > sub.SubscriptionCount
  // So publisher has many subscribers, but each subscriber has just 1 subscription
  PublisherVar := nil;
  Sub1 := TBoldPassthroughSubscriber.Create(HandleReceive);
  Sub2 := TBoldPassthroughSubscriber.Create(HandleReceive2);
  Sub3 := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    PublisherVar := TBoldPublisher.Create(PublisherVar);
    try
      // 3 subscribers on one publisher → pub count(3) > sub count(1 each)
      PublisherVar.AddSmallSubscription(Sub1, [beValueChanged], beDefaultRequestedEvent);
      PublisherVar.AddSmallSubscription(Sub2, [beValueChanged], beDefaultRequestedEvent);
      PublisherVar.AddSmallSubscription(Sub3, [beValueChanged], beDefaultRequestedEvent);

      // Cancel Sub2 — triggers subscriber-side loop path
      PublisherVar.CancelSubscriptionTo(Sub2);

      PublisherVar.SendEvent(beValueChanged);
      Assert.AreEqual(2, FReceiveCallCount, 'Two remaining subscribers should receive event');
    finally
      PublisherVar.NotifySubscribersAndClearSubscriptions(nil);
      PublisherVar.Free;
    end;
  finally
    Sub1.Free;
    Sub2.Free;
    Sub3.Free;
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

{ Post-Notification Queue — Immediate Execution }

procedure TTestBoldSubscription.TestDelayTillAfterNotification_Immediate;
begin
  // When G_NotificationNesting = 0 (outside any notification),
  // BoldAddEventToPostNotifyQueue should execute the handler immediately (line 1307)
  BoldAddEventToPostNotifyQueue(HandleDelayedAction, nil, Self);
  Assert.AreEqual(1, FDelayedActionCallCount,
    'Handler should execute immediately when not inside a notification');
end;

procedure TTestBoldSubscription.TestBoldForcedDequeuePostNotify;
var
  PublisherVar: TBoldPublisher;
begin
  // Test BoldForcedDequeuePostNotify (lines 515-524):
  // Queue a delayed action during StartNotify, then force dequeue before EndNotify
  PublisherVar := nil;
  PublisherVar := TBoldPublisher.Create(PublisherVar);
  try
    TBoldPublisher.StartNotify;
    try
      // Queue delayed action — should NOT execute yet (nesting > 0)
      BoldAddEventToPostNotifyQueue(HandleDelayedAction, nil, Self);
      Assert.AreEqual(0, FDelayedActionCallCount,
        'Handler should not execute while inside StartNotify');

      // Force dequeue while still inside notification
      PublisherVar.BoldForcedDequeuePostNotify;
      Assert.AreEqual(1, FDelayedActionCallCount,
        'Handler should execute after BoldForcedDequeuePostNotify');
    finally
      TBoldPublisher.EndNotify;
    end;
  finally
    PublisherVar.Free;
  end;
end;

{ Deduplication — Subscriber-Side Paths }

procedure TTestBoldSubscription.TestSmallEventDeduplication_SubscriberSidePath;
var
  PublisherVar: TBoldPublisher;
  Sub1, Sub2: TBoldPassthroughSubscriber;
begin
  // To trigger subscriber-side dedup loop (lines 781-782):
  // pubCount > subCount, so publisher has more subscriptions than subscriber
  // We need 2 subscribers on publisher (pubCount=2), and Sub1 has only 1 subscription (subCount=1)
  PublisherVar := nil;
  Sub1 := TBoldPassthroughSubscriber.Create(HandleReceive);
  Sub2 := TBoldPassthroughSubscriber.Create(HandleReceive2);
  try
    PublisherVar := TBoldPublisher.Create(PublisherVar);
    try
      // Add Sub1 and Sub2 so publisher has 2 subscriptions
      PublisherVar.AddSmallSubscription(Sub1, [beValueChanged], beDefaultRequestedEvent);
      PublisherVar.AddSmallSubscription(Sub2, [beItemAdded], beDefaultRequestedEvent);

      // Now pubCount(2) > Sub1.subCount(1) → subscriber-side dedup path
      // Re-add Sub1 with different events but same RequestedEvent → should extend
      PublisherVar.AddSmallSubscription(Sub1, [beItemDeleted], beDefaultRequestedEvent);

      Assert.AreEqual(2, PublisherVar.SubscriptionCount,
        'Should still be 2 subscriptions (Sub1 extended, not duplicated)');

      // Verify both original and extended events work for Sub1
      PublisherVar.SendEvent(beValueChanged);
      PublisherVar.SendEvent(beItemDeleted);
      Assert.AreEqual(2, FReceiveCallCount,
        'Sub1 should receive both original and extended events');
    finally
      PublisherVar.NotifySubscribersAndClearSubscriptions(nil);
      PublisherVar.Free;
    end;
  finally
    Sub1.Free;
    Sub2.Free;
  end;
end;

procedure TTestBoldSubscription.TestBigEventDeduplication_SubscriberSidePath;
var
  PublisherVar: TBoldPublisher;
  Sub1, Sub2: TBoldPassthroughSubscriber;
begin
  // To trigger subscriber-side big event dedup (lines 825-834):
  // pubCount > subCount, publisher has more subscriptions than subscriber
  PublisherVar := nil;
  Sub1 := TBoldPassthroughSubscriber.Create(HandleReceive);
  Sub2 := TBoldPassthroughSubscriber.Create(HandleReceive2);
  try
    PublisherVar := TBoldPublisher.Create(PublisherVar);
    try
      // Add Sub1 (big event) and Sub2 so publisher has 2 subscriptions
      PublisherVar.AddSubscription(Sub1, boeClassChanged, beDefaultRequestedEvent);
      PublisherVar.AddSubscription(Sub2, boeClassChanged, beDefaultRequestedEvent);

      // Now pubCount(2) > Sub1.subCount(1) → subscriber-side loop
      // Re-add same big event for Sub1 → should deduplicate (exit early)
      PublisherVar.AddSubscription(Sub1, boeClassChanged, beDefaultRequestedEvent);

      Assert.AreEqual(2, PublisherVar.SubscriptionCount,
        'Big event dedup should not create a third subscription');

      PublisherVar.SendExtendedEvent(nil, boeClassChanged, []);
      Assert.AreEqual(1, FReceiveCallCount, 'Sub1 should receive exactly one event');
      Assert.AreEqual(1, FReceiveCallCount2, 'Sub2 should receive exactly one event');
    finally
      PublisherVar.NotifySubscribersAndClearSubscriptions(nil);
      PublisherVar.Free;
    end;
  finally
    Sub1.Free;
    Sub2.Free;
  end;
end;

{ Publisher Diagnostics }

procedure TTestBoldSubscription.TestPublisher_ContextString_WithSubscribableObject;
var
  PublisherVar: TBoldPublisher;
  Obj: TObject;
begin
  // Test ContextString when SubscribableObject is set to a plain TObject (line 612)
  PublisherVar := nil;
  Obj := TObject.Create;
  try
    PublisherVar := TBoldPublisher.Create(PublisherVar);
    try
      PublisherVar.SubscribableObject := Obj;
      Assert.AreEqual('TObject', PublisherVar.ContextString,
        'ContextString should return ClassName of SubscribableObject');

      // Also verify DebugInfo returns the same (lines 618-620)
      Assert.AreEqual(PublisherVar.ContextString, PublisherVar.DebugInfo,
        'DebugInfo should equal ContextString');
    finally
      PublisherVar.NotifySubscribersAndClearSubscriptions(nil);
      PublisherVar.Free;
    end;
  finally
    Obj.Free;
  end;
end;

{ Subscriber Diagnostics — Additional Paths }

procedure TTestBoldSubscription.TestSubscriber_ContextString_TComponentOwner;
var
  Comp: TTestHelperComponent;
  Subscriber: TBoldPassthroughSubscriber;
begin
  // Test ContextString when method owner is a TComponent (line 981)
  Comp := TTestHelperComponent.Create(nil);
  try
    Comp.Name := 'MyTestComp';
    Subscriber := TBoldPassthroughSubscriber.Create(Comp.HandleReceive);
    try
      Assert.AreEqual('MyTestComp', Subscriber.ContextString,
        'ContextString should return TComponent.Name when method owner is TComponent');
    finally
      Subscriber.Free;
    end;
  finally
    Comp.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscriber_BaseContextString;
var
  Subscriber: TBoldSubscriberAccess;
begin
  // Test base TBoldSubscriber.GetContextString (lines 1192-1194)
  // TBoldSubscriberAccess does NOT override GetContextString, so it uses the base
  // which returns ClassName
  Subscriber := TBoldSubscriberAccess.Create;
  try
    Assert.AreEqual('TBoldSubscriberAccess', Subscriber.ContextString,
      'Base GetContextString should return ClassName');
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscription.TestSubscriber_SubscriptionsAsText_PlainObject;
var
  PublisherVar: TBoldPublisher;
  Obj: TObject;
  Subscriber: TBoldPassthroughSubscriber;
  Text: string;
begin
  // Test subscriber's SubscriptionsAsText when SubscribableObject is a plain TObject (line 1240)
  // This hits the else branch: SubscribableObject.ClassName
  PublisherVar := nil;
  Obj := TObject.Create;
  try
    Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
    try
      PublisherVar := TBoldPublisher.Create(PublisherVar);
      try
        PublisherVar.SubscribableObject := Obj;
        PublisherVar.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);

        Text := Subscriber.SubscriptionsAsText;
        Assert.IsNotEmpty(Text, 'SubscriptionsAsText should be non-empty');
        Assert.Contains(Text, 'TObject',
          'Should contain TObject class name for plain object SubscribableObject');
      finally
        PublisherVar.NotifySubscribersAndClearSubscriptions(nil);
        PublisherVar.Free;
      end;
    finally
      Subscriber.Free;
    end;
  finally
    Obj.Free;
  end;
end;

{ SubscribableObject Diagnostics }

procedure TTestBoldSubscription.TestSubscribableObject_SubscriptionsAsText;
var
  Obj: TBoldSubscribableObject;
  Subscriber: TBoldPassthroughSubscriber;
  Text: string;
begin
  // Test TBoldSubscribableObject.SubscriptionsAsText (lines 1025-1027)
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    Obj := TBoldSubscribableObject.Create;
    try
      Obj.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);
      Text := Obj.SubscriptionsAsText;
      Assert.IsNotEmpty(Text, 'SubscriptionsAsText should be non-empty');
      Assert.Contains(Text, '0:', 'Should contain subscription index');
    finally
      Obj.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

{ Array Growth }

procedure TTestBoldSubscription.TestManySubscriptions_TriggersLargeGrowth;
var
  PublisherVar: TBoldPublisher;
  Subscribers: array[0..69] of TBoldPassthroughSubscriber;
  i: Integer;
begin
  // Test GetNewLength > 64 branch (line 486): adding 65+ subscriptions
  // Each subscriber gets a unique RequestedEvent to prevent deduplication
  PublisherVar := nil;
  for i := 0 to High(Subscribers) do
    Subscribers[i] := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    PublisherVar := TBoldPublisher.Create(PublisherVar);
    try
      for i := 0 to High(Subscribers) do
        PublisherVar.AddSmallSubscription(Subscribers[i], [beValueChanged], i + 1);

      Assert.AreEqual(Length(Subscribers), PublisherVar.SubscriptionCount,
        'Should have 70 subscriptions');

      // Send event to verify all still work
      FReceiveCallCount := 0;
      PublisherVar.SendEvent(beValueChanged);
      Assert.AreEqual(Length(Subscribers), FReceiveCallCount,
        'All 70 subscribers should receive the event');
    finally
      PublisherVar.NotifySubscribersAndClearSubscriptions(nil);
      PublisherVar.Free;
    end;
  finally
    for i := 0 to High(Subscribers) do
      Subscribers[i].Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldSubscription);

end.
