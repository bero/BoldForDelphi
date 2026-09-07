# TBoldObjectReference

Reference to a Bold object

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldObjectReference = class(TBoldMember)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldFlaggedObject`
4. [TBoldSubscribableObject](../BoldSubscription/TBoldSubscribableObject.md)
5. [TBoldElement](../BoldElements/TBoldElement.md)
6. [TBoldDomainElement](../BoldDomainElement/TBoldDomainElement.md)
7. [TBoldMember](TBoldMember.md)
8. TBoldObjectReference

## Description

A TBoldObjectReference is a member that holds a reference to a Bold object. It is primarily used for roles with [0..1] multiplicity. Like [TBoldObjectList](TBoldObjectList.md), it has additional methods for manipulating the Bold object by its locator. This is more efficient since the Bold object does not have to be loaded into memory.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [BoldObject](#boldobject) | The object the reference refers to |  |
| [BoldRoleRTInfo](#boldrolertinfo) | The model information for the object reference | read-only |
| [HasOldValues](#hasoldvalues) | Bold-internal |  |
| [Locator](#locator) | The locator of the referred object |  |
| [OldEmbeddingOtherEndId](#oldembeddingotherendid) | Bold-internal | read-only |

### BoldObject

```delphi
property BoldObject: TBoldObject;
```

The object the reference refers to

### BoldRoleRTInfo

```delphi
property BoldRoleRTInfo: TBoldRoleRTInfo;
```

This property is the same as [BoldMemberRtInfo](TBoldMember.md#boldmemberrtinfo), casted to `TBoldRoleRTInfo`.

### HasOldValues

```delphi
property HasOldValues: Boolean;
```

Bold-internal

### Locator

```delphi
property Locator: TBoldObjectLocator;
```

It can be more efficient to access the `Locator` property, than the [BoldObject](TBoldObjectReference.md#boldobject) property, as it will not require the object to be fetched.

### OldEmbeddingOtherEndId

```delphi
property OldEmbeddingOtherEndId: TBoldObjectId;
```

Bold-internal

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Assign](#assign) | Overrides TBoldElement.Assign | override |
| [CanClear](#canclear) | If it is allowed to Clear the reference |  |
| [CanSet](#canset) | If it is allowed to set the object reference to the NewObject |  |
| [CanSetLocator](#cansetlocator) | Same as CanSet, but takes a locator instead. |  |
| [Clear](#clear) | Set the object reference to nil. |  |
| [CompareToAs](#comparetoas) | Overrides TBoldElement.CompareToAs | override |
| [CreateTypedReference](#createtypedreference) | Construct a TBoldObjectReference for Bold objects of type ObjectClass. |  |
| [DefaultSubscribe](#defaultsubscribe) | Overrides TBoldElement.DefaultSubscribe | override |
| [Destroy](#destroy) | Destructor | override |
| [GetAsList](#getaslist) | Overrides TBoldElement.GetAsList | override |
| [GetStreamName](#getstreamname) | Overrides TBoldMember.GetStreamName | protected, override |
| [GetStringRepresentation](#getstringrepresentation) | Overrides TBoldElement.GetStringRepresentation | protected, override |
| [InitializeMember](#initializemember) | Overrides TBoldMember.InitializeMember | protected, override |
| [IsEqualAs](#isequalas) | Overrides TBoldElement.IsEqualAs | override |
| [IsEqualToValue](#isequaltovalue) | Overrides TBoldMember.IsEqualToValue | override |
| [ObserverMayModify](#observermaymodify) | Overrides TBoldElement.ObserverMayModify | override |
| [ProxyClass](#proxyclass) | Overrides TBoldMember.ProxyClass | protected, override |
| [ProxyInterface](#proxyinterface) | Overrides TBoldElement.ProxyClass | override |
| [SetStringRepresentation](#setstringrepresentation) | Overrides TBoldElement.SetStringRepresentation | protected, override |
| [SubscribeToStringRepresentation](#subscribetostringrepresentation) | Overrides TBoldElement.SubscribeToStringRepresentation | override |

### Assign

```delphi
procedure Assign(Source: TBoldElement); override; See also Ancestor Method
```

A TBoldObjectReference can be assigned another TBoldObjectReference, or a TBoldObject.

### CanClear

```delphi
function CanClear(Subscriber: TBoldSubscriber): Boolean;
```

You can make CanClear return false, and thus prohibit Clear on a TBoldObjectReference, by subscribing to the bqMayClear query and returning false.

### CanSet

```delphi
function CanSet(NewObject: TBoldObject; Subscriber: TBoldSubscriber): Boolean;
```

Returns `True` if it is allowed to set the object reference to the NewObject. A programmer can disallow setting by subscribing to the `bqMaySet` query. If a `Subscriber` is passed to the function, subscriptions will be placed to notify when the result of `CanSet` may have changed. Note that the parameter to the `bqMaySet` query will be the locator of the NewObject, and not the NewObject itself.

**See Also**

- Subscriptions

### CanSetLocator

```delphi
function CanSetLocator(NewLocator: TBoldObjectLocator; Subscriber: TBoldSubscriber): Boolean;
```

Returns `True` if it is allowed to set the object reference to the Bold object of NewLocator. A programmer can disallow setting by subscribing to the `bqMaySet` query. If a `Subscriber` is passed to the function, subscriptions will be placed to notify when the result of `CanSetLocator` may have changed.

**See Also**

- Subscriptions

### Clear

```delphi
procedure Clear;
```

Set the object reference to `**nil**`.

### CompareToAs

```delphi
function CompareToAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Integer; override; See also Ancestor Method
```

Overrides [CompareToAs](../BoldElements/TBoldElement.md#comparetoas). An object reference can be compared with another object reference, or a `TBoldObject`. The result is the same as comparing the objects, except if one of the references is `**nil**`. `**Nil**` is considered smaller than any object.

### CreateTypedReference

```delphi
constructor CreateTypedReference(ObjectClass: TBoldObjectClass);
```

Construct a TBoldObjectReference for Bold objects of type ObjectClass.

### DefaultSubscribe

```delphi
procedure DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override; See also Ancestor Method
```

The default events for a `TBoldObjectReference` are `beValueChanged` and `beValueInvalid`.

**See Also**

- Subscriptions

### Destroy

```delphi
destructor Destroy; override; See also Ancestor Method
```

Destructor

### GetAsList

```delphi
procedure GetAsList(ResultList: TBoldIndirectElement); override; See also Ancestor Method
```

Will result in a [TBoldObjectList](TBoldObjectList.md) containing the Bold object that the references refers to. If the reference is `**nil**`, the list will be empty.

### GetStreamName

```delphi
function GetStreamName: String; override; See also Ancestor Method
```

Overrides [GetStreamName](TBoldMember.md#getstreamname)

### GetStringRepresentation

```delphi
function GetStringRepresentation(Representation: TBoldRepresentation): string; override; See also Ancestor Method
```

The string representation of a `TBoldObjectReference` is the string representation of the object it refers, or the empty string if `**nil**`

### InitializeMember

```delphi
procedure InitializeMember(AOwningElement: TBoldDomainElement; ElementTypeInfo: TBoldElementTypeInfo); override; See also Ancestor Method
```

Overrides [TBoldMember](TBoldMember.md)

### IsEqualAs

```delphi
function IsEqualAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Boolean; override; See also Ancestor Method
```

A reference is considered equal if the object it refers is equal. Also, a `**nil**` reference is equal to another `**nil**` reference, or to `**nil**`.

### IsEqualToValue

```delphi
function IsEqualToValue(Value: IBoldValue): Boolean; override; See also Ancestor Method
```

Overrides [IsEqualToValue](TBoldMember.md#isequaltovalue). In contrast to [IsEqualAs](TBoldObjectReference.md#isequalas), `IsEqualToValue` is only true if the references refer the same object, but not if two different objects are equal (using IsEqual).

### ObserverMayModify

```delphi
function ObserverMayModify(Observer: TObject): Boolean; override; See also Ancestor Method
```

Overrides [ObserverMayModify](../BoldElements/TBoldElement.md#observermaymodify)

### ProxyClass

```delphi
function ProxyClass: TBoldMember_ProxyClass; override; See also Ancestor Method
```

Overrides [ProxyClass](TBoldMember.md#proxyclass)

### ProxyInterface

```delphi
function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override; See also Ancestor Method
```

Overrides ProxyClass

### SetStringRepresentation

```delphi
procedure SetStringRepresentation(Representation: TBoldRepresentation; Value: string); override; See also Ancestor Method
```

This sets the string representation of the referred object.

### SubscribeToStringRepresentation

```delphi
procedure SubscribeToStringRepresentation(Representation: TBoldRepresentation; Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override; See also Ancestor Method
```

Overrides [SubscribeToStringRepresentation](../BoldElements/TBoldElement.md#subscribetostringrepresentation)

**See Also**

- Subscriptions

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
