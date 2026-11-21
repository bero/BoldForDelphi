# TBoldPassthroughSubscriber

`TBoldPassthroughSubscriber` is the most commonly used concrete subscriber. It passes each event it receives on to a
method, so you can react to Bold events without writing a subscriber class.

## Class Hierarchy

```mermaid
classDiagram
    TBoldSubscriber <|-- TBoldPassthroughSubscriber
    TBoldPassthroughSubscriber <|-- TBoldExtendedPassthroughSubscriber

    TBoldPassthroughSubscriber : +Create(AReceiveFunc)
    TBoldPassthroughSubscriber : +ReceiveFunc
    TBoldExtendedPassthroughSubscriber : +CreateWithExtendedReceive()
    TBoldExtendedPassthroughSubscriber : +CreateWithReceiveAndAnswer()

    click TBoldSubscriber href "../TBoldSubscriber/" "TBoldSubscriber documentation"
```

## Class Definition

```pascal
TBoldEventHandler = procedure(Originator: TObject;
  OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent) of object;

TBoldPassthroughSubscriber = class(TBoldSubscriber)
public
  constructor Create(AReceiveFunc: TBoldEventHandler);
  property ReceiveFunc: TBoldEventHandler read/write;
end;

// Extended version for events with arguments and queries
TBoldExtendedPassthroughSubscriber = class(TBoldPassthroughSubscriber)
public
  constructor CreateWithExtendedReceive(
    AExtendedReceiveFunc: TBoldExtendedEventHandler);
  constructor CreateWithReceiveAndAnswer(
    AReceiveFunc: TBoldEventHandler;
    AAnswerFunc: TBoldQueryHandler);
end;
```

## Working with TBoldPassthroughSubscriber

### Basic Event Handling

```pascal
type
  TMyForm = class(TForm)
  private
    fSubscriber: TBoldPassthroughSubscriber;
    procedure HandleEvent(Originator: TObject;
      OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent);
  end;

procedure TMyForm.FormCreate(Sender: TObject);
begin
  // Create the subscriber with its callback
  fSubscriber := TBoldPassthroughSubscriber.Create(HandleEvent);

  // Subscribe to value changes of one attribute
  Customer.M_Name.DefaultSubscribe(fSubscriber);
end;

procedure TMyForm.HandleEvent(Originator: TObject;
  OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent);
begin
  // React to the change
  UpdateDisplay;
end;

procedure TMyForm.FormDestroy(Sender: TObject);
begin
  fSubscriber.Free; // cancels its subscriptions
end;
```

### Subscribing to Several Events

```pascal
// Subscribe to small events using a set
Customer.AddSmallSubscription(fSubscriber,
  [beValueChanged, beObjectDeleted],
  beValueChanged); // RequestedEvent, for routing
```

### Extended Events with Arguments

```pascal
fExtSubscriber := TBoldExtendedPassthroughSubscriber.CreateWithExtendedReceive(
  HandleExtendedEvent);

procedure TMyForm.HandleExtendedEvent(Originator: TObject;
  OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  // Args hold additional event data
end;
```

### Query Handling (Veto Pattern)

```pascal
fQuerySubscriber := TBoldExtendedPassthroughSubscriber.CreateWithReceiveAndAnswer(
  HandleEvent, HandleQuery);

function TMyForm.HandleQuery(Originator: TObject;
  OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent;
  const Args: array of const;
  Subscriber: TBoldSubscriber): Boolean;
begin
  // Return False to veto the operation
  Result := CanPerformOperation;
end;
```

## Subscribable Base Classes

Most Bold classes inherit from a subscribable base class that owns a `TBoldPublisher`:

| Base Class | Inherits From | Used By |
|------------|---------------|---------|
| `TBoldSubscribableObject` | `TBoldFlaggedObject` | Elements: objects, members, the system |
| `TBoldSubscribableComponent` | `TComponent` | Handles and other non-visual Bold components |
| `TBoldSubscribablePersistent` | `TPersistent` | Follower controllers (the `BoldProperties` of controls) |

All three have `AddSubscription(Subscriber, Event, RequestedEvent)` and
`AddSmallSubscription(Subscriber, EventSet, RequestedEvent)`. To send an event, `TBoldSubscribableObject` has
`SendEvent(Event)`; the other two take the originator as well, `SendEvent(Originator, Event)`.

## See Also

- [TBoldSubscriber](TBoldSubscriber.md) - Abstract base
- [TBoldPublisher](TBoldPublisher.md) - The sender side
- [TBoldAbstractDeriver](TBoldAbstractDeriver.md) - Derivation subscriber
- [Subscriptions](../concepts/subscriptions.md) - Concept overview
- [TBoldPassthroughSubscriber in the Class Reference](../reference/BoldSubscription/TBoldPassthroughSubscriber.md) - All members (2004 help)
