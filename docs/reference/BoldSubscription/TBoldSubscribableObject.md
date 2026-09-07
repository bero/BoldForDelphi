# TBoldSubscribableObject

TBoldSubscribableComponent is an abstract superclass for classes that need to be subscribable.

**Unit**: [BoldSubscription](index.md)

## Declaration

```delphi
TBoldSubscribableObject = class(TBoldFlaggedObject)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldFlaggedObject`
4. TBoldSubscribableObject
5. **Direct subclasses**
6. `TBoldClient`
7. `TBoldComClientConnection`
8. [TBoldElement](../BoldElements/TBoldElement.md)
9. `TBoldLogHandler`
10. `TBoldPersistenceController`
11. `TBoldPersistenceMapper`
12. `TBoldRegionDefinitions`
13. `TBoldServer`

## Description

It has the same interface as a TBoldPublisher, i.e. SendEvent and AddSubscription.

| **Note** |
|---|

| It is implemented by allocating an internal TBoldPublisher and passing the calls on to it. |
|---|

## Properties

| Name | Summary | Notes |
|---|---|---|
| [HasSubscribers](#hassubscribers) |  | protected, read-only |
| [Publisher](#publisher) |  | protected, read-only |

### HasSubscribers

```delphi
property HasSubscribers: Boolean;
```

### Publisher

```delphi
property Publisher: TBoldPublisher;
```

## Methods

| Name | Summary | Notes |
|---|---|---|
| [AddSmallSubscription](#addsmallsubscription) |  |  |
| [AddSubscription](#addsubscription) |  |  |
| [Destroy](#destroy) |  | override |
| [FreePublisher](#freepublisher) |  | protected |
| [SendEvent](#sendevent) | SendEvent notifies all subscribers holding a subscription matching Event by calling their Receive method. | virtual |
| [SendExtendedEvent](#sendextendedevent) | Sends an extended event to all subscribers. | virtual |
| [SendQuery](#sendquery) | Sends a query to all subscribers | virtual |

### AddSmallSubscription

```delphi
procedure AddSmallSubscription(Subscriber: TBoldSubscriber; Events: TBoldSmallEventSet; RequestedEvent: TBoldRequestedEvent); See Also - AddSmallSubscription
```

### AddSubscription

```delphi
procedure AddSubscription(Subscriber: TBoldSubscriber; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent); See Also - AddSubscription
```

### Destroy

```delphi
destructor Destroy; override;
```

The destructor will free the internal TBoldPublisher.

### FreePublisher

```delphi
procedure FreePublisher;
```

### SendEvent

```delphi
procedure SendEvent(OriginalEvent: TBoldEvent); virtual;
```

If a subscriber holds multiple subscriptions matching the same event, but with different requested events, Receive will be called multiple times, once for each requested event.

This method is virtual to allow the concrete subclasses to perform other actions when sending an event.

### SendExtendedEvent

```delphi
procedure SendExtendedEvent(OriginalEvent: TBoldEvent; const Args: array of const); virtual;
```

An extended event can pass any arguments to the subscribers. Each event should send a well defined set of arguments that the subscribers can extract additional information from.

### SendQuery

```delphi
function SendQuery(OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean; virtual;
```

A subscriber can subscribe to a query event. Call SendQuery to query all subscribers to the particular query event to see if any subscriber has any objection to the query. If the result of SendQuery is false, this should be interpreted as a veto from one of the subscribers.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
