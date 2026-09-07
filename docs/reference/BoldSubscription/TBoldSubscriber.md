# TBoldSubscriber

TBoldSubscriber is an abstract superclass for classes subscribing to events.

**Unit**: [BoldSubscription](index.md)

## Declaration

```delphi
TBoldSubscriber = class(TBoldMemoryManagedObject)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. TBoldSubscriber
4. **Direct subclasses**
5. `TBoldAbstractDeriver`
6. `TBoldComClientSubscriber`
7. `TBoldComServerSubscriber`
8. `TBoldFollowerSubscriber`
9. `TBoldLogReceiverSubscriber`
10. [TBoldPassthroughSubscriber](TBoldPassthroughSubscriber.md)
11. `TBoldRegion`

## Description

Concrete classes become subscribable either by subclassing TBoldSubscriber, e.g. TBoldFollower, or by using a TBoldPassthroughSubscriber.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [ContextString](#contextstring) |  | read-only |
| [HandlesExtendedEvents](#handlesextendedevents) |  | read-only |

### ContextString

```delphi
property ContextString: string;
```

This property returns a string describing the subscriber. The default implementation returns '', this can be changed by overriding [GetContextString](TBoldSubscriber.md#getcontextstring).

### HandlesExtendedEvents

```delphi
property HandlesExtendedEvents: Boolean;
```

This property is used by the subscription mechanism to know if [Receive](TBoldSubscriber.md#receive) or [ReceiveExtended](TBoldSubscriber.md#receiveextended) should be called when notifying a subscriber.

| **Note** |
|---|

| Default is to return false, override the [GethandlesExtendedEvents](TBoldSubscriber.md#gethandlesextendedevents) to change this |
|---|

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Answer](#answer) |  | protected, virtual |
| [CancelAllSubscriptions](#cancelallsubscriptions) |  |  |
| [CloneSubscriptions](#clonesubscriptions) |  |  |
| [Destroy](#destroy) |  | override |
| [GetContextString](#getcontextstring) |  | protected, virtual |
| [GetHandlesExtendedEvents](#gethandlesextendedevents) |  | protected, virtual |
| [Receive](#receive) | This method is called each time a event occurs that the subscriber holds a subscription to occurs. | protected, abstract |
| [ReceiveExtended](#receiveextended) |  | protected, virtual |

### Answer

```delphi
function Answer(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean; virtual;
```

Each time SendQuery is called on a TBoldPublisher, TBoldSubscriber_Answer is be called on each subscriber holding a subscription matching the event. If all Answer calls return True, SendQuery will return True. If any Answer returns False, SendQuery returns False. The process is terminated on the first False reply, so there is no guarantee that Answer will be called for all subscribers.

This function is only called by TBoldPublisher.

| Parameters |
|---|

| Originator | The object originating the event, given as parameter to SendQuery. |
|---|---|
| OriginalEvent | The event send by Originator |
| RequestedEvent | The requested event given when placing the subscription with AddSubscription or AddSmallSubscription. |

### CancelAllSubscriptions

```delphi
procedure CancelAllSubscriptions;
```

This method cancels all the subscribers subscriptions.

### CloneSubscriptions

```delphi
procedure CloneSubscriptions(Subscriber: TBoldSubscriber; OldRequestedEvent: TBoldRequestedEvent; NewRequestedEvent: TBoldRequestedEvent);
```

### Destroy

```delphi
destructor Destroy; override;
```

### GetContextString

```delphi
function GetContextString: string; virtual;
```

Default implementation of property [ContextString](TBoldSubscriber.md#contextstring), returns ''

### GetHandlesExtendedEvents

```delphi
function GetHandlesExtendedEvents: Boolean; virtual;
```

Implements the property [HandlesExtendedEvents](TBoldSubscriber.md#handlesextendedevents). The base implementation always returns false, but subclasses may override this.

### Receive

```delphi
procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent); virtual; abstract;
```

Receive is an abstract method that must be implemented in each concrete subclass. This procedure is only called by TBoldPublisher.

| Parameters |
|---|

| OriginalEvent | the event that triggers the subscription |
|---|---|
| Originator | the object on which OriginalEvent occurred |
| RequestedEvent | the event requested when placing the subscription with AddSubscription or AddSmallSubscription |

### ReceiveExtended

```delphi
procedure ReceiveExtended(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); virtual;
```

This method is similar to [Receive](TBoldSubscriber.md#receive), but it also get a set of arguments that were provided by the object who sent the event. Subscribers that implements ReceiveExtended should always override the [GetHandlesExtendedEvents](TBoldSubscriber.md#gethandlesextendedevents) otherwise this method will never be called.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
