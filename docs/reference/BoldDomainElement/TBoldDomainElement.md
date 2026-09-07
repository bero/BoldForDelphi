# TBoldDomainElement

Superclass of all objects that represent business data

**Unit**: [BoldDomainElement](index.md)

## Declaration

```delphi
TBoldDomainElement = class(TBoldElement)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldFlaggedObject`
4. [TBoldSubscribableObject](../BoldSubscription/TBoldSubscribableObject.md)
5. [TBoldElement](../BoldElements/TBoldElement.md)
6. TBoldDomainElement
7. **Direct subclasses**
8. [TBoldMember](../BoldSystem/TBoldMember.md)
9. [TBoldObject](../BoldSystem/TBoldObject.md)
10. [TBoldSystem](../BoldSystem/TBoldSystem.md)

## Description

TBoldDomainElement is the superclass of all objects that represent business data, such as objects, attributes and associations.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [BoldDirty](#bolddirty) |  | read-only |
| [BoldPersistent](#boldpersistent) |  | read-only |
| [DisplayName](#displayname) |  | read-only |
| [OwningElement](#owningelement) | The owner of the element | read-only |

### BoldDirty

```delphi
property BoldDirty: Boolean;
```

This flag indicates whether the element has changes relative to the persistent storage, and is in need to be updated.

### BoldPersistent

```delphi
property BoldPersistent: Boolean;
```

This flag indicates whether the element is persistent. This flag is derived from settings in the model. A system is persistent if the model is marked as persistent, and the system has a persistence controller. An object is persistent if it is marked as persistent, and the system is persistent An attribute is persistent if it is marked as persistent, and belongs to an object that is persistent. A relation is persistent if it is marked as persistent and it connects two persistent objects.

### DisplayName

```delphi
property DisplayName: String;
```

Name of the model element of the element The display name is a string representing the model element of the domain element. For instance, an attribute might have the display name 'MyClass.MyAttribute'. It is primarily useful for debugging purposes.

### OwningElement

```delphi
property OwningElement: TBoldDomainElement;
```

Domain elements can have an owner. For instance, Bold objects (TBoldObject) will be owned by the system, and attributes will be owned by their Bold objects.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [CanCommit](#cancommit) | If the element allows the current system-transaction to commit. |  |
| [Create](#create) |  | virtual |
| [GetBoldDirty](#getbolddirty) | Get-method for the BoldDirty property | protected, abstract |
| [GetDisplayName](#getdisplayname) | Get-method for the DisplayName property | protected, abstract |
| [MayCommit](#maycommit) | Return false to prohibit a transaction from committing. | protected, virtual |
| [ProxyInterface](#proxyinterface) | Bold-internal equivalent of QueryInterface | virtual |
| [ReceiveEventFromOwned](#receiveeventfromowned) | Event sent from owned element | protected, virtual |
| [ReceiveQueryFromOwned](#receivequeryfromowned) | Query sent from owned element | protected, virtual |
| [SendEvent](#sendevent) | Sends events to subscribers and owner | override |
| [SendExtendedEvent](#sendextendedevent) | Sends extended events to subscribers and owner | override |
| [SendQuery](#sendquery) | Sends queries to subscribers and owner | override |
| [StateError](#stateerror) | Bold internal | protected, virtual |

### CanCommit

```delphi
function CanCommit: Boolean;
```

This method is called on each involved domain element when a system-transaction is committed. If the result is false, the transaction is not allowed to commit. The user can veto the commit by overriding [MayCommit](TBoldDomainElement.md#maycommit) or by subscribing to the bqMayCommit query.

### Create

```delphi
constructor Create(OwningElement: TBoldDomainElement); virtual;
```

Since TBoldDomainElement is abstract, there is no point in calling this constructor. It will be called by subclasses to set the owner of the object.

### GetBoldDirty

```delphi
function GetBoldDirty: Boolean; virtual; abstract;
```

Get-method for the [BoldDirty](TBoldDomainElement.md#bolddirty) property

### GetDisplayName

```delphi
function GetDisplayName: String; virtual; abstract;
```

Get-method for the [DisplayName](TBoldDomainElement.md#displayname) property

### MayCommit

```delphi
function MayCommit: Boolean; virtual;
```

A user may override this method and return false to affect the result of [CanCommit](TBoldDomainElement.md#cancommit).

### ProxyInterface

```delphi
function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; virtual;
```

This method is used internally in bold as an alternative to QueryInterface. Custom attribute classes may need to override it.

### ReceiveEventFromOwned

```delphi
procedure ReceiveEventFromOwned(Originator: TObject; OriginalEvent: TBoldEvent); virtual;
```

This method receives events sent by an owned element. See [SendEvent](TBoldDomainElement.md#sendevent).

### ReceiveQueryFromOwned

```delphi
function ReceiveQueryFromOwned(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean; virtual;
```

This method receives queries sent by an owned element. See [SendQuery](TBoldDomainElement.md#sendquery).

### SendEvent

```delphi
procedure SendEvent(OriginalEvent: TBoldEvent); override; See also Ancestor Method
```

This is an override of [SendEvent](../BoldSubscription/TBoldSubscribableObject.md#sendevent) and sends the event to the [ReceiveEventFromOwned](TBoldDomainElement.md#receiveeventfromowned) method of the OwnedElement. The normal sending, as per the subscription mechanism, still takes place.

### SendExtendedEvent

```delphi
procedure SendExtendedEvent(OriginalEvent: TBoldEvent; const Args: array of const); override; See also Ancestor Method
```

This is an override of [SendExtendedEvent](../BoldSubscription/TBoldSubscribableObject.md#sendextendedevent) and sends the extended event to the [ReceiveEventFromOwned](TBoldDomainElement.md#receiveeventfromowned) method of the OwnedElement. The normal sending, as per the subscription mechanism, still takes place.

### SendQuery

```delphi
function SendQuery(OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean; override; See also Ancestor Method
```

This is an override of [SendQuery](../BoldSubscription/TBoldSubscribableObject.md#sendquery) and sends the event to the [ReceiveQueryFromOwned](TBoldDomainElement.md#receivequeryfromowned) method of the OwnedElement. The normal sending, as per the subscription mechanism, still takes place.

### StateError

```delphi
procedure StateError(S: string); virtual;
```

Bold internal

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
