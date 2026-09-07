# TBoldSubscribableComponent

TBoldSubscribableComponent is an abstract superclass for classes that need to be subclasses to TComponent and also need to be subscribable.

**Unit**: [BoldSubscription](index.md)

## Declaration

```delphi
TBoldSubscribableComponent = class(TComponent)
```

## Hierarchy

1. TComponent
2. TBoldSubscribableComponent
3. **Direct subclasses**
4. `TBoldAbstractDatabaseAdapter`
5. `TBoldAbstractDequeuer`
6. `TBoldAbstractLockManagerAdminHandle`
7. `TBoldAbstractLockManagerHandle`
8. `TBoldAbstractModel`
9. `TBoldAbstractPropagatorHandle`
10. `TBoldAbstractXMLProducer`
11. [TBoldElementHandle](../BoldHandles/TBoldElementHandle.md)
12. [TBoldHandle](../BoldHandle/TBoldHandle.md)
13. `TBoldHTTPServerPersistenceHandlePassthrough`
14. `TBoldLockingHandle`
15. `TBoldOclRepository`
16. `TBoldOclVariables`
17. `TBoldRendererCom`
18. [TBoldSubscribableComponentViaBoldElem](../BoldElements/TBoldSubscribableComponentViaBoldElem.md)
19. `TBoldVariableDefinitionCom`

## Description

It has essentially the same interface as a TBoldPublisher.

| **Note** |
|---|

| It is implemented by allocating an internal TBoldPublisher and passing the calls on to it. |
|---|

## Properties

| Name | Summary | Notes |
|---|---|---|
| [HasSubscribers](#hassubscribers) |  | read-only |
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
| [GetHasSubscribers](#gethassubscribers) |  | protected, virtual |
| [SendEvent](#sendevent) |  |  |
| [SendExtendedEvent](#sendextendedevent) | Sends an extended event to all subscribers. |  |
| [SendQuery](#sendquery) | Sends a query to all subscribers |  |

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

### FreePublisher

```delphi
procedure FreePublisher;
```

### GetHasSubscribers

```delphi
function GetHasSubscribers: Boolean; virtual;
```

### SendEvent

```delphi
procedure SendEvent(Originator: TObject; OriginalEvent: TBoldEvent);
```

SendEvent notifies all subscribers holding a subscription matching Event by calling their [Receive](TBoldSubscriber.md#receive) method. If a subscriber holds multiple subscriptions matching the same event, but with different requested events, Receive will be called multiple times, once for each requested event.

### SendExtendedEvent

```delphi
procedure SendExtendedEvent(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const);
```

An extended event can pass any arguments to the subscribers. Each event should send a well defined set of arguments that the subscribers can extract additional information from.

### SendQuery

```delphi
function SendQuery(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
```

A subscriber can subscribe to a query event. Call SendQuery to query all subscribers to the particular query event to see if any subscriber has any objection to the query. If the result of SendQuery is false, this should be interpreted as a veto from one of the subscribers.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
