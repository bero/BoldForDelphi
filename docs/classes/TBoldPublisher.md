# TBoldPublisher

`TBoldPublisher` is the sender side of Bold's subscription (observer) pattern. It holds a list of subscriptions and
sends events to the subscribers. Bold objects, members and handles create one on their first subscription;
application code rarely creates a publisher itself.

## Class Hierarchy

```mermaid
classDiagram
    TBoldMemoryManagedObject <|-- TBoldPublisher
    TBoldPublisher o-- TBoldSubscriber : notifies

    TBoldPublisher : +AddSubscription()
    TBoldPublisher : +AddSmallSubscription()
    TBoldPublisher : +SendEvent()
    TBoldPublisher : +SendQuery()
    TBoldPublisher : +HasSubscribers

    click TBoldSubscriber href "../TBoldSubscriber/" "TBoldSubscriber documentation"
```

## Class Definition

```pascal
TBoldPublisher = class(TBoldMemoryManagedObject)
public
  constructor Create(var APublisherReference);
  destructor Destroy; override;
  procedure AddSubscription(Subscriber: TBoldSubscriber;
    OriginalEvent: TBoldEvent;
    RequestedEvent: TBoldRequestedEvent = beDefaultRequestedEvent);
  procedure AddSmallSubscription(Subscriber: TBoldSubscriber;
    Events: TBoldSmallEventSet;
    RequestedEvent: TBoldRequestedEvent = beDefaultRequestedEvent);
  procedure SendEvent(OriginalEvent: TBoldEvent);
  procedure SendExtendedEvent(Originator: TObject;
    OriginalEvent: TBoldEvent; const Args: array of const);
  function SendQuery(Originator: TObject; OriginalEvent: TBoldEvent;
    const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
  procedure CancelSubscriptionTo(Subscriber: TBoldSubscriber);
  procedure NotifySubscribersAndClearSubscriptions(Originator: TObject);
  property HasSubscribers: Boolean;
  property SubscriptionCount: Integer;
  property SubscriptionsAsText: string; // debug
end;
```

## Event System

Events are integers (`TBoldEvent = 0..100000`). Small events (0–29) can be combined in sets, so one subscription
can cover several of them.

### Core Event Constants

| Event | Value | Meaning |
|-------|-------|---------|
| `beDestroying` | 0 | Object being destroyed |
| `beMemberChanged` | 1 | A member value changed |
| `beObjectDeleted` | 2 | Object was deleted |
| `beObjectCreated` | 3 | New object created |
| `beItemAdded` | 4 | Item added to list |
| `beItemDeleted` | 5 | Item removed from list |
| `beItemReplaced` | 6 | Item replaced in list |
| `beValueChanged` | 8 | Attribute or reference value changed |
| `breReSubscribe` | 10 | Request to resubscribe |
| `beValueInvalid` | 12 | Value marked invalid |

### Sending Methods

| Method | Purpose |
|--------|---------|
| `SendEvent` | One-way notification; the publisher itself is the `Originator` |
| `SendExtendedEvent` | Notification with an explicit `Originator` and extra arguments |
| `SendQuery` | Two-way: subscribers can veto the operation |

## How It Works

```mermaid
sequenceDiagram
    participant P as Publisher
    participant S1 as Subscriber 1
    participant S2 as Subscriber 2

    S1->>P: AddSubscription(beValueChanged)
    S2->>P: AddSmallSubscription([beItemAdded, beItemDeleted])
    Note over P: Value changes...
    P->>S1: Receive(beValueChanged)
    Note over P: Item added...
    P->>S2: Receive(beItemAdded)
```

## Common Patterns

### Subscribable Base Classes

Most Bold classes already own a publisher through `TBoldSubscribableObject`, `TBoldSubscribableComponent` or
`TBoldSubscribablePersistent`. Use their methods:

```pascal
// TBoldSubscribableObject (objects, members):
MyObject.AddSubscription(Subscriber, beValueChanged);
MyObject.SendEvent(beValueChanged);

// TBoldSubscribableComponent (handles):
MyComponent.AddSmallSubscription(Subscriber, [beItemAdded, beItemDeleted]);
MyComponent.SendEvent(Self, beItemAdded);
```

### Direct Usage (Rare)

A class that cannot inherit from one of those base classes can own a publisher. The destructor must clear the
subscriptions before it frees the publisher; `TBoldPublisher.Destroy` asserts that none are left.

```pascal
type
  TMyClass = class
  private
    fPublisher: TBoldPublisher;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Changed;
    property Publisher: TBoldPublisher read fPublisher;
  end;

constructor TMyClass.Create;
begin
  inherited;
  fPublisher := TBoldPublisher.Create(fPublisher);
end;

destructor TMyClass.Destroy;
begin
  fPublisher.NotifySubscribersAndClearSubscriptions(Self); // sends beDestroying
  FreeAndNil(fPublisher);
  inherited;
end;

procedure TMyClass.Changed;
begin
  fPublisher.SendExtendedEvent(Self, beValueChanged, []);
end;
```

## See Also

- [TBoldSubscriber](TBoldSubscriber.md) - The receiver side
- [TBoldPassthroughSubscriber](TBoldPassthroughSubscriber.md) - Concrete subscriber
- [Subscriptions](../concepts/subscriptions.md) - Concept overview
- [TBoldPublisher in the Class Reference](../reference/BoldSubscription/TBoldPublisher.md) - All members (2004 help)
