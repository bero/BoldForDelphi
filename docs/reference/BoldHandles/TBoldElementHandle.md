# TBoldElementHandle

Super class to handles that hold a value

**Unit**: [BoldHandles](index.md)

## Declaration

```delphi
TBoldElementHandle = class(TBoldSubscribableComponent)
```

## Hierarchy

1. TComponent
2. [TBoldSubscribableComponent](../BoldSubscription/TBoldSubscribableComponent.md)
3. TBoldElementHandle
4. **Direct subclasses**
5. [TBoldAbstractSystemHandle](TBoldAbstractSystemHandle.md)
6. `TBoldFormSaver`
7. [TBoldNonSystemHandle](TBoldNonSystemHandle.md)
8. [TBoldSystemTypeInfoHandle](TBoldSystemTypeInfoHandle.md)

## Description

The element-handles in Bold (i.e. the subclasses to `TBoldElementHandle`) are the Delphi IDE programmer's main tool for accessing the domain objects in a system using Bold. The handles themselves have several main functions:

- Holding a value
- Allowing Chained Evaluation of OCL-expressions
- Holding a type, in the case when the value is `**nil**`

**Value**

The most important property of a handle is [Value](TBoldElementHandle.md#value) which refers to a [TBoldElement](../BoldElements/TBoldElement.md) in the domain object layer of the system. The different types of handles get their value in different ways. The value can be `**nil**`.

**Typing**  
Each handle also has a number of properties related to the (potential) type of `Value`. This type information is used both for design-time support (i.e. for the OCL-editor), and by components that change their behavior dynamically depending on the type, such as the grid with automatic columns. There are three basic type properties:

- [StaticBoldType](TBoldElementHandle.md#staticboldtype): A type that is defined on the handle at design-time, and which therefore is known whether the handle has a value or not.
- [DynamicBoldType](TBoldElementHandle.md#dynamicboldtype): The type of Value, and thus `**nil**` if `Value` is `**nil**`.
- [BoldType](TBoldElementHandle.md#boldtype): "The best you can get", i.e. `DynamicBoldType` if (`Value <> **nil**`), otherwise `StaticBoldType`

**Bold events**

`TBoldElementHandle` is a subclass of `TBoldSubscribableComponent`, and can therefore by subscribed to using `AddSmallSubscription`. A `TBoldElementHandle` can send the following events:

- beDestroying: Sent when the handle is about to be destroyed
- beValueIdentityChanged: Sent when `Value` has changed, i.e. when `Value` points to a new `TBoldElement`. Also sent if anything influencing `BoldStaticType` has changed. The event will not be sent if it has previously been sent, and the Value property has not been accessed since then. Note that the event is not send when the contents of Value is changed. This is found out by subscribing to `Value` itself.

It is important to note that there is a difference between the fact that the `Value` property changes, and that the contents of the element referred to by `Value` changes.

Any event leading to a (possible) change in `Value` will lead to a `beValueIdentityChanged` event being sent to all the subscribers of the handle.

Any event leading to a possible change of the contents of `Value` will lead to the subscribers to `Value` being notified according to their subscriptions.

If a convenient way is needed to subscribe to the `Value` of a handle, without worrying about these two types of subscriptions, a `TBoldPlaceableSubscriber` should be used.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [BoldType](#boldtype) | The type of the Value property, or the static type if Value is nil | read-only |
| [DynamicBoldType](#dynamicboldtype) | The type of Value, or nil if Value is nil | read-only |
| [StaticBoldType](#staticboldtype) | The type of Value, as defined at design-time. | read-only |
| [StaticSystemTypeInfo](#staticsystemtypeinfo) | Best static determination of which type system the Value will belong to. | read-only |
| [StrictType](#stricttype) | Ensure checking of type for handle. |  |
| [Value](#value) | The value of the handle. | read-only |

### BoldType

```delphi
property BoldType: TBoldElementTypeInfo;
```

The type of the [Value](TBoldElementHandle.md#value) property, or the static type if `Value` is `**nil**`.

It is primarily intended to be used e.g. by Bold-aware controls that have some dynamic behavior based on the type of the handle they are pointing at, such as the automatic column headers on a grid. `BoldType` is `DynamicBoldType` if `Value <> **nil**`, otherwise it is `StaticBoldType`.

**Bold Events**  
Since `BoldType` is a readonly property setting it can't directly give rise to events. The following event will however be sent when the value of the property has changed (or rather may have changed, since it may have "changed" to the same value):

- **beValueIndentityChanged**: Since `BoldType` depends on `Value` and `StaticBoldType` this event will be sent whenever `BoldType` can have changed.

The event will not be sent if it has previously been sent, and the Value property has not been accessed since then.

### DynamicBoldType

```delphi
property DynamicBoldType: TBoldElementTypeInfo;
```

The type of [Value](TBoldElementHandle.md#value), or `**nil**` if `Value` is currently `**nil**`. In most cases [BoldType](TBoldElementHandle.md#boldtype) is more interesting.

**Bold Events**

Since `BoldType` is a read only property setting it can't directly give rise to events. The following event will however be sent when the value of the property has changed (or rather may have changed, since it may have "changed" to the same value):

- **beValueIndentityChanged**: Since Dynamic BoldType depends on Value this event will be sent whenever `DynamicBoldType` can have changed.

The event may not be sent if it has previously been sent, and the Value property has not been accessed since then.

### StaticBoldType

```delphi
property StaticBoldType: TBoldElementTypeInfo;
```

The type of [Value](TBoldElementHandle.md#value), as defined at design-time. Different concrete subclasses of `TBoldElementHandle` get their static type in different ways. For each handle type this is described on the overview of the class.

**Bold events**

Since `StaticBoldType` is a read only property setting it can't directly give rise to events. The following event will however be sent when the value of the property has changed (or rather may have changed, since it may have "changed" to the same value):

- **beValueIndentityChanged**: Will be sent whenever the conditions defining `StaticBoldType` have changed.

### StaticSystemTypeInfo

```delphi
property StaticSystemTypeInfo: TBoldSystemTypeInfo;
```

For all [TBoldNonSystemHandle](TBoldNonSystemHandle.md)s it is determined from [StaticSystemHandle](TBoldNonSystemHandle.md#staticsystemhandle) if given, otherwise the SystemTypeInfo of the [DefaultBoldSystemHandle](TBoldAbstractSystemHandle.md#defaultboldsystemhandle) is used.

This property is mainly intended for the internal use of the handle when defining [StaticBoldType](TBoldElementHandle.md#staticboldtype) and for property-editors used in defining the type.

### StrictType

```delphi
property StrictType: Boolean;
```

Enforce type on handle. If `StrictType` is `true`, an exception will be raised if [DynamicBoldType](TBoldElementHandle.md#dynamicboldtype) of the handle is not a subtype of [StaticBoldType](TBoldElementHandle.md#staticboldtype).

| **Warning** |
|---|

| This property has not been implemented yet |
|---|

### Value

```delphi
property Value: TBoldElement;
```

The `Value` of the handle is always a `TBoldElement`. Different concrete subclasses of `TBoldElement` get their value in different ways. For each handle type this is described on the overview of the class.

A `TBoldElementHandle` will always subscribe to `beDestroying` of its `Value` (regardless of how it is obtained), so that if the element is removed, `Value` will be set to `**nil**`, and a `beValueIdentityChanged` will be sent to all subscribers of the handle.

**Bold events**

Since `Value` is a read only property (for all subclasses except `TBoldReferenceHandle`), setting it can't directly give rise to events. The following event will however be sent when the value of the property has changed (or rather may have changed, since it may have "changed" to the same value):

- beValueIndentityChanged: Sent when `Value` has changed, i.e. when `Value` points to a new `TBoldElement`.

| **Note** |
|---|

| Note that the event is not send when the contents of `Value` is changed. This is found out by subscribing to `Value` itself. |
|---|

The event will not be sent if it has previously been sent, and the `Value` property has not been accessed since then.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Destroy](#destroy) | Destroys an instance of a TBoldElementHandle | override |
| [GetStaticBoldType](#getstaticboldtype) | Get-method for the StaticBoldType property | protected, abstract |
| [GetStaticSystemTypeInfo](#getstaticsystemtypeinfo) | Get-method for the StaticSystemTypeInfo property | protected, abstract |
| [GetValue](#getvalue) | Get-method for the Value property | protected, abstract |
| [RefersToComponent](#referstocomponent) | Used to determine relationships between a TBoldElementHandle and the component passed as parameter | virtual |
| [StaticBoldTypeChanged](#staticboldtypechanged) | Bold-internal | protected, virtual |

### Destroy

```delphi
destructor Destroy; override; See also Ancestor Method
```

`Destroy` destroys an instance of TBoldElementHandle. Do not call `Destroy` directly, use `Free` instead.

### GetStaticBoldType

```delphi
function GetStaticBoldType: TBoldElementTypeInfo; virtual; abstract;
```

Get-method for the [StaticBoldType](TBoldElementHandle.md#staticboldtype) property

### GetStaticSystemTypeInfo

```delphi
function GetStaticSystemTypeInfo: TBoldSystemTypeInfo; virtual; abstract;
```

Get-method for the [StaticSystemTypeInfo](TBoldElementHandle.md#staticsystemtypeinfo) property

### GetValue

```delphi
function GetValue: TBoldElement; virtual; abstract;
```

Get-method for the [Value](TBoldElementHandle.md#value) property

### RefersToComponent

```delphi
function RefersToComponent(Component: TBoldSubscribableComponent): Boolean; virtual;
```

`RefersToComponent` is used internally to avoid circular relations between handles and other components.

### StaticBoldTypeChanged

```delphi
procedure StaticBoldTypeChanged; virtual;
```

Bold-internal

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
