# TBoldPassthroughSubscriber

**Unit**: [BoldSubscription](index.md)

## Declaration

```delphi
TBoldPassthroughSubscriber = class(TBoldSubscriber)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. [TBoldSubscriber](TBoldSubscriber.md)
4. TBoldPassthroughSubscriber

## Description

TBoldPassthroughSubscriber is a subscriber intended for classes that do not subclass TBoldSubscriber directly, but still need to subscribe to events. It simply passes the call to Receive on to a method in another object.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Answer](#answer) |  | protected, override |
| [Create](#create) |  |  |
| [CreateWithExtendedReceive](#createwithextendedreceive) |  |  |
| [CreateWithReceiveAndAnswer](#createwithreceiveandanswer) |  |  |
| [GetHandlesExtendedEvents](#gethandlesextendedevents) |  | protected, override |
| [Receive](#receive) |  | protected, override |
| [ReceiveExtended](#receiveextended) |  | protected, override |

### Answer

```delphi
function Answer(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean; override; See also Ancestor Method
```

### Create

```delphi
constructor Create(receiveFunc: TBoldEventHandler);
```

ReceiveFunc is a pointer to the method to which calls to Receive will be passed on.

### CreateWithExtendedReceive

```delphi
constructor CreateWithExtendedReceive(ExtendedReceiveFunc: TBoldExtendedEventHandler);
```

### CreateWithReceiveAndAnswer

```delphi
constructor CreateWithReceiveAndAnswer(ReceiveFunc: TBoldEventHandler; AnswerFunc: TBoldQueryHandler);
```

### GetHandlesExtendedEvents

```delphi
function GetHandlesExtendedEvents: Boolean; override; See also Ancestor Method
```

### Receive

```delphi
procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent); override; See also Ancestor Method
```

### ReceiveExtended

```delphi
procedure ReceiveExtended(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override; See also Ancestor Method
```

## Events

| Name | Summary | Notes |
|---|---|---|
| [receiveFunc](#receivefunc) |  |  |

### receiveFunc

```delphi
TBoldEventHandler = procedure(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent) of object;
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
