# TBoldPublisher

**Unit**: [BoldSubscription](index.md)

## Declaration

```delphi
TBoldPublisher = class(TBoldMemoryManagedObject)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. TBoldPublisher

## Description

Developers will not use a TBoldPublisher directly except when Making_a_Class_Subscribable_Task. In most normal use, the methods on TBoldSubscribableObject will be used.

TBoldPublisher performs the publishing part of the Subscription_Mechanism.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [HasSubscribers](#hassubscribers) | HasSubscribers is true if the publisher has any subscribers. | read-only |

### HasSubscribers

```delphi
property HasSubscribers: Boolean;
```

HasSubscribers is true if the publisher has any subscribers.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [AddSmallSubscription](#addsmallsubscription) |  |  |
| [AddSubscription](#addsubscription) |  |  |
| [Create](#create) |  |  |
| [Destroy](#destroy) | Destructor for TBoldPublisher. | override |
| [EndNotify](#endnotify) |  |  |
| [GetPublisherFlag](#getpublisherflag) |  | protected |
| [NotifySubscribersAndClearSubscriptions](#notifysubscribersandclearsubscriptions) | The method will perform a SendEvent (Originator, beDestroying), and subsequently clear all subscriptions. |  |
| [SendExtendedEvent](#sendextendedevent) | A call to this method will lead to TBoldSubscriber_Receive beeing called for all subscribers holding a subscription mathching OriginalEvent. |  |
| [SendQuery](#sendquery) |  |  |
| [SetPublisherFlag](#setpublisherflag) |  | protected |
| [StartNotify](#startnotify) |  |  |

### AddSmallSubscription

```delphi
procedure AddSmallSubscription(Subscriber: TBoldSubscriber; Events: TBoldSmallEventSet; RequestedEvent: TBoldRequestedEvent);
```

| **Note** |
|---|

| If Subscriber already has a subscription with the same RequestedEvent, Events will be merged into that subscription. A consequence of this is that subscribing to the same event multiple times with the same RequestedEvent will only give one subscription. |
|---|

| Parameters |
|---|

| Subscriber | The subscriber to be notified if TBoldPublisher_SendEvent or TBoldPublisher_SendQuery is called with an event matching Event. |
|---|---|
| Events | The set of events to match |
| RequestedEvent | The value send as the RequestedEvent parameter to TBoldSubscriber_Receive or TBoldSubscriber_Answer. |

### AddSubscription

```delphi
procedure AddSubscription(Subscriber: TBoldSubscriber; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
```

| **Note** |
|---|

| If Subscriber already has a subscription with the same RequestedEventand and the same OriginalEvent, no additional subsctiption is placed. |
|---|

| Parameters |
|---|

| Subscriber | The subscriber to be notified if SendEvent or [SendQuery](TBoldPublisher.md#sendquery) is called with an event matching Event. |
|---|---|
| OriginalEvent | The set of events to match |
| RequestedEvent | The value send as the RequestedEvent parameter to [Receive](TBoldSubscriber.md#receive) or [Answer](TBoldSubscriber.md#answer). |

### Create

```delphi
constructor Create;
```

### Destroy

```delphi
destructor Destroy; override;
```

Will raise an exception if the publicher still has subscribers. Subscribers should be cleared prior to destruction by calling [NotifySubscribersAndClearSubscriptions](TBoldPublisher.md#notifysubscribersandclearsubscriptions).

### EndNotify

```delphi
class procedure EndNotify;
```

### GetPublisherFlag

```delphi
function GetPublisherFlag(Flag: TBoldPublisherFlag): Boolean;
```

### NotifySubscribersAndClearSubscriptions

```delphi
procedure NotifySubscribersAndClearSubscriptions(Originator: TObject);
```

In general this method will be called by the object using the publisher early in the destructor, in order to prepare all subscribers for the destruction, and ensure that no spurious events are generated during the destruction process.

| Parameters |
|---|

| Originator | The objects to be sent as the Originator to SendEvent. In general the object using the publisher to become subscribable |
|---|---|

### SendExtendedEvent

```delphi
procedure SendExtendedEvent(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const); | Parameters | |---| | Originator | The objects to be sent as the Originator parameter to Receive. | |---|---| | OriginalEvent | The value used as the OriginalEvent parameter to Receive. |
```

A call to this method will lead to TBoldSubscriber_Receive beeing called for all subscribers holding a subscription mathching OriginalEvent.

### SendQuery

```delphi
function SendQuery(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
```

### SetPublisherFlag

```delphi
procedure SetPublisherFlag(Flag: TBoldPublisherFlag; Value: Boolean);
```

### StartNotify

```delphi
class procedure StartNotify;
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
