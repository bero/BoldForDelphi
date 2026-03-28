unit Test.BoldSubscribableCollection;

interface

uses
  DUnitX.TestFramework,
  BoldSubscription,
  BoldSubscribableCollection;

type
  [TestFixture]
  TTestBoldSubscribableCollection = class
  private
    FReceivedOriginators: array of TObject;
    FReceivedOriginalEvents: array of TBoldEvent;
    FReceiveCallCount: Integer;
    FExtendedCallCount: Integer;
    FExtendedArgCount: Integer;
    FAnswerResult: Boolean;
    FAnswerCallCount: Integer;
    FDestroyEventReceived: Boolean;

    procedure HandleReceive(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent);
    procedure HandleReceiveCheckDestroying(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent);
    procedure HandleExtendedReceive(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent; const Args: array of const);
    function HandleAnswer(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent; const Args: array of const;
      Subscriber: TBoldSubscriber): Boolean;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    { TBoldSubscribableCollectionItem }
    [Test]
    procedure TestCollectionItem_Create;
    [Test]
    procedure TestCollectionItem_SubscribeAndReceive;
    [Test]
    procedure TestCollectionItem_SendExtendedEvent;
    [Test]
    procedure TestCollectionItem_SendQuery;
    [Test]
    procedure TestCollectionItem_DestroyNotifiesSubscribers;
    [Test]
    procedure TestCollectionItem_HasSubscribers;

    { Extended TBoldSubscribableCollection coverage }
    [Test]
    procedure TestCollection_SendExtendedEvent;
    [Test]
    procedure TestCollection_SendQuery;
    [Test]
    procedure TestCollection_AddSubscription;
    [Test]
    procedure TestCollection_FreePublisher;
  end;

implementation

uses
  SysUtils,
  Classes;

{ TTestBoldSubscribableCollection }

procedure TTestBoldSubscribableCollection.Setup;
begin
  FReceiveCallCount := 0;
  FExtendedCallCount := 0;
  FExtendedArgCount := 0;
  FAnswerResult := True;
  FAnswerCallCount := 0;
  FDestroyEventReceived := False;
  SetLength(FReceivedOriginators, 0);
  SetLength(FReceivedOriginalEvents, 0);
end;

procedure TTestBoldSubscribableCollection.TearDown;
begin
  SetLength(FReceivedOriginators, 0);
  SetLength(FReceivedOriginalEvents, 0);
end;

procedure TTestBoldSubscribableCollection.HandleReceive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
var
  Len: Integer;
begin
  Inc(FReceiveCallCount);
  Len := Length(FReceivedOriginators);
  SetLength(FReceivedOriginators, Len + 1);
  SetLength(FReceivedOriginalEvents, Len + 1);
  FReceivedOriginators[Len] := Originator;
  FReceivedOriginalEvents[Len] := OriginalEvent;
end;

procedure TTestBoldSubscribableCollection.HandleReceiveCheckDestroying(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if OriginalEvent = beDestroying then
    FDestroyEventReceived := True;
end;

procedure TTestBoldSubscribableCollection.HandleExtendedReceive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  Inc(FExtendedCallCount);
  FExtendedArgCount := Length(Args);
end;

function TTestBoldSubscribableCollection.HandleAnswer(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
begin
  Inc(FAnswerCallCount);
  Result := FAnswerResult;
end;

{ TBoldSubscribableCollectionItem tests }

procedure TTestBoldSubscribableCollection.TestCollectionItem_Create;
var
  Coll: TBoldSubscribableCollection;
  Item: TBoldSubscribableCollectionItem;
begin
  Coll := TBoldSubscribableCollection.Create(TBoldSubscribableCollectionItem);
  try
    Item := TBoldSubscribableCollectionItem.Create(Coll);
    Assert.IsNotNull(Item, 'CollectionItem should be created');
    Assert.AreEqual(1, Coll.Count, 'Collection should have one item');
    Assert.IsFalse(Item.HasSubscribers, 'New item should have no subscribers');
  finally
    Coll.Free;
  end;
end;

procedure TTestBoldSubscribableCollection.TestCollectionItem_SubscribeAndReceive;
var
  Coll: TBoldSubscribableCollection;
  Item: TBoldSubscribableCollectionItem;
  Subscriber: TBoldPassthroughSubscriber;
begin
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    Coll := TBoldSubscribableCollection.Create(TBoldSubscribableCollectionItem);
    try
      Item := TBoldSubscribableCollectionItem.Create(Coll);
      Item.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);
      Assert.IsTrue(Item.HasSubscribers, 'Item should have subscribers');

      Item.SendEvent(beValueChanged);
      Assert.AreEqual(1, FReceiveCallCount, 'Subscriber should receive event from item');
      Assert.AreSame(Item, FReceivedOriginators[0], 'Originator should be the collection item');
    finally
      Coll.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscribableCollection.TestCollectionItem_SendExtendedEvent;
var
  Coll: TBoldSubscribableCollection;
  Item: TBoldSubscribableCollectionItem;
  Subscriber: TBoldExtendedPassthroughSubscriber;
begin
  Subscriber := TBoldExtendedPassthroughSubscriber.CreateWithExtendedReceive(HandleExtendedReceive);
  try
    Coll := TBoldSubscribableCollection.Create(TBoldSubscribableCollectionItem);
    try
      Item := TBoldSubscribableCollectionItem.Create(Coll);
      Item.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);

      Item.SendExtendedEvent(beValueChanged, [42, 'hello']);

      Assert.AreEqual(1, FExtendedCallCount, 'Extended receive should be called');
      Assert.AreEqual(2, FExtendedArgCount, 'Should receive 2 args');
    finally
      Coll.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscribableCollection.TestCollectionItem_SendQuery;
var
  Coll: TBoldSubscribableCollection;
  Item: TBoldSubscribableCollectionItem;
  Subscriber: TBoldExtendedPassthroughSubscriber;
  QueryResult: Boolean;
begin
  FAnswerResult := True;
  Subscriber := TBoldExtendedPassthroughSubscriber.CreateWithReceiveAndAnswer(HandleReceive, HandleAnswer);
  try
    Coll := TBoldSubscribableCollection.Create(TBoldSubscribableCollectionItem);
    try
      Item := TBoldSubscribableCollectionItem.Create(Coll);
      Item.AddSubscription(Subscriber, bqMayModify, bqMayModify);

      QueryResult := Item.SendQuery(bqMayModify, [], nil);
      Assert.IsTrue(QueryResult, 'Query should be approved');
      Assert.AreEqual(1, FAnswerCallCount, 'Answer should be called once');

      // Test veto
      FAnswerResult := False;
      QueryResult := Item.SendQuery(bqMayModify, [], nil);
      Assert.IsFalse(QueryResult, 'Query should be vetoed');
    finally
      Coll.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscribableCollection.TestCollectionItem_DestroyNotifiesSubscribers;
var
  Coll: TBoldSubscribableCollection;
  Item: TBoldSubscribableCollectionItem;
  Subscriber: TBoldPassthroughSubscriber;
begin
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceiveCheckDestroying);
  try
    Coll := TBoldSubscribableCollection.Create(TBoldSubscribableCollectionItem);
    try
      Item := TBoldSubscribableCollectionItem.Create(Coll);
      Item.AddSmallSubscription(Subscriber, [beDestroying], beDefaultRequestedEvent);

      // Destroying collection destroys items, which should notify subscribers
      FreeAndNil(Coll);
    except
      FreeAndNil(Coll);
      raise;
    end;

    Assert.IsTrue(FDestroyEventReceived, 'Destroying collection item should send beDestroying');
    Assert.AreEqual(0, Subscriber.SubscriptionCount, 'Subscriber should have no subscriptions after item destroyed');
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscribableCollection.TestCollectionItem_HasSubscribers;
var
  Coll: TBoldSubscribableCollection;
  Item: TBoldSubscribableCollectionItem;
  Subscriber: TBoldPassthroughSubscriber;
begin
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    Coll := TBoldSubscribableCollection.Create(TBoldSubscribableCollectionItem);
    try
      Item := TBoldSubscribableCollectionItem.Create(Coll);

      Assert.IsFalse(Item.HasSubscribers, 'Should not have subscribers initially');

      Item.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);
      Assert.IsTrue(Item.HasSubscribers, 'Should have subscribers after subscription');
    finally
      Coll.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

{ Extended TBoldSubscribableCollection tests }

procedure TTestBoldSubscribableCollection.TestCollection_SendExtendedEvent;
var
  Coll: TBoldSubscribableCollection;
  Subscriber: TBoldExtendedPassthroughSubscriber;
begin
  Subscriber := TBoldExtendedPassthroughSubscriber.CreateWithExtendedReceive(HandleExtendedReceive);
  try
    Coll := TBoldSubscribableCollection.Create(TCollectionItem);
    try
      Coll.AddSmallSubscription(Subscriber, [beValueChanged], beDefaultRequestedEvent);
      Coll.SendExtendedEvent(beValueChanged, [99, 'data']);

      Assert.AreEqual(1, FExtendedCallCount, 'Extended receive should be called');
      Assert.AreEqual(2, FExtendedArgCount, 'Should receive 2 args');
    finally
      Coll.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscribableCollection.TestCollection_SendQuery;
var
  Coll: TBoldSubscribableCollection;
  Subscriber: TBoldExtendedPassthroughSubscriber;
  QueryResult: Boolean;
begin
  FAnswerResult := False;
  Subscriber := TBoldExtendedPassthroughSubscriber.CreateWithReceiveAndAnswer(HandleReceive, HandleAnswer);
  try
    Coll := TBoldSubscribableCollection.Create(TCollectionItem);
    try
      Coll.AddSubscription(Subscriber, bqMayModify, bqMayModify);

      QueryResult := Coll.SendQuery(bqMayModify, [], nil);
      Assert.IsFalse(QueryResult, 'Query should be vetoed');
      Assert.AreEqual(1, FAnswerCallCount, 'Answer should be called');

      // No subscribers => approved
      Subscriber.CancelAllSubscriptions;
      QueryResult := Coll.SendQuery(bqMayModify, [], nil);
      Assert.IsTrue(QueryResult, 'Query with no subscribers should return True');
    finally
      Coll.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscribableCollection.TestCollection_AddSubscription;
var
  Coll: TBoldSubscribableCollection;
  Subscriber: TBoldPassthroughSubscriber;
begin
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceive);
  try
    Coll := TBoldSubscribableCollection.Create(TCollectionItem);
    try
      // AddSubscription with a big event (non-small)
      Coll.AddSubscription(Subscriber, boeClassChanged, beDefaultRequestedEvent);
      Assert.IsTrue(Coll.HasSubscribers, 'Collection should have subscribers');

      Coll.SendEvent(boeClassChanged);
      Assert.AreEqual(1, FReceiveCallCount, 'Subscriber should receive big event from collection');
    finally
      Coll.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldSubscribableCollection.TestCollection_FreePublisher;
var
  Coll: TBoldSubscribableCollection;
  Subscriber: TBoldPassthroughSubscriber;
begin
  Subscriber := TBoldPassthroughSubscriber.Create(HandleReceiveCheckDestroying);
  try
    Coll := TBoldSubscribableCollection.Create(TCollectionItem);
    try
      Coll.AddSmallSubscription(Subscriber, [beDestroying], beDefaultRequestedEvent);
      Assert.IsTrue(Coll.HasSubscribers, 'Should have subscribers');

      // Destroying collection calls FreePublisher internally
    finally
      Coll.Free;
    end;

    Assert.IsTrue(FDestroyEventReceived, 'FreePublisher should send beDestroying');
    Assert.AreEqual(0, Subscriber.SubscriptionCount, 'Subscriber should be cleared after FreePublisher');
  finally
    Subscriber.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldSubscribableCollection);

end.
