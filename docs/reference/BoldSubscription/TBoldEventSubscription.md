# TBoldEventSubscription

!!! warning "Not in the current source"
    This class is documented in the Bold 4.0 help, but no class or interface with this name
    is declared in `Source/` today. It was removed or reshaped (for example into a record).

**Unit**: [BoldSubscription](index.md)

## Declaration

```delphi
TBoldEventSubscription = class(TBoldSubscription)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. [TBoldSubscription](TBoldSubscription.md)
4. TBoldEventSubscription

## Methods

| Name | Summary | Notes |
|---|---|---|
| [CloneTo](#cloneto) |  | protected, override |
| [Create](#create) |  |  |
| [IsMatchingEvent](#ismatchingevent) |  | protected, override |

### CloneTo

```delphi
procedure CloneTo(Subscriber: TBoldSubscriber; NewRequestedEvent: TBoldRequestedEvent); override; See also Ancestor Method
```

### Create

```delphi
constructor Create(Publisher: TBoldPublisher; Subscriber: TBoldSubscriber; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
```

### IsMatchingEvent

```delphi
function IsMatchingEvent(OriginalEvent: TBoldEvent): Boolean; override; See also Ancestor Method
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
