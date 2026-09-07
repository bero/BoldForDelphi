# TBoldSubscription

!!! warning "Not in the current source"
    This class is documented in the Bold 4.0 help, but no class or interface with this name
    is declared in `Source/` today. It was removed or reshaped (for example into a record).

**Unit**: [BoldSubscription](index.md)

## Declaration

```delphi
TBoldSubscription = class(TBoldMemoryManagedObject)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. TBoldSubscription
4. **Direct subclasses**
5. [TBoldEventSubscription](TBoldEventSubscription.md)
6. [TBoldSmallEventSubscription](TBoldSmallEventSubscription.md)

## Description

TBoldSubscription is an abstract superclass. The actual subscriptions are the concrete subclasses of TBoldSubscription. A TBoldSubscription has 4 basic properties, a [TBoldPublisher](TBoldPublisher.md), a [TBoldSubscriber](TBoldSubscriber.md), a match condition and a requested event. The requested event allows mapping of one or more actual events an event meaningful for the Subscriber. Each subscription defines the conditions for notifying the subscriber. They will normally be a TBoldEvent or a set of TBoldSmallEvents. When SendEvent is called it checks all the publishers subscriptions. For each subscription that matches the event [Receive](TBoldSubscriber.md#receive) is called for the corresponding subscriber. The same is done for [SendQuery](TBoldPublisher.md#sendquery) and [Answer](TBoldSubscriber.md#answer)

## Properties

| Name | Summary | Notes |
|---|---|---|
| [Publisher](#publisher) |  | protected, read-only |
| [RequestedEvent](#requestedevent) |  | protected, read-only |
| [Subscriber](#subscriber) |  | protected, read-only |

### Publisher

```delphi
property Publisher: TBoldPublisher;
```

### RequestedEvent

```delphi
property RequestedEvent: TBoldRequestedEvent;
```

### Subscriber

```delphi
property Subscriber: TBoldSubscriber;
```

## Methods

| Name | Summary | Notes |
|---|---|---|
| [CloneTo](#cloneto) |  | protected, abstract |
| [Create](#create) |  | protected |
| [IsMatchingEvent](#ismatchingevent) |  | protected, abstract |
| [UnlinkFromPublisher](#unlinkfrompublisher) |  | protected |
| [UnlinkFromSubscriber](#unlinkfromsubscriber) |  | protected |

### CloneTo

```delphi
procedure CloneTo(Subscriber: TBoldSubscriber; NewRequestedEvent: TBoldRequestedEvent); virtual; abstract;
```

### Create

```delphi
constructor Create(Publisher: TBoldPublisher; Subscriber: TBoldSubscriber; RequestedEvent: TBoldRequestedEvent);
```

### IsMatchingEvent

```delphi
function IsMatchingEvent(OriginalEvent: TBoldEvent): Boolean; virtual; abstract;
```

### UnlinkFromPublisher

```delphi
procedure UnlinkFromPublisher;
```

### UnlinkFromSubscriber

```delphi
procedure UnlinkFromSubscriber;
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
