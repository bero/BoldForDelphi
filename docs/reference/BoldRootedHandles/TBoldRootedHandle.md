# TBoldRootedHandle

TBoldRootedHandle is the superclass for all handles that get their value and type by deriving it from another handle (the [RootHandle](TBoldRootedHandle.md#roothandle)).

**Unit**: [BoldRootedHandles](index.md)

## Declaration

```delphi
TBoldRootedHandle = class(TBoldNonSystemHandle)
```

## Hierarchy

1. TComponent
2. [TBoldSubscribableComponent](../BoldSubscription/TBoldSubscribableComponent.md)
3. [TBoldElementHandle](../BoldHandles/TBoldElementHandle.md)
4. [TBoldNonSystemHandle](../BoldHandles/TBoldNonSystemHandle.md)
5. TBoldRootedHandle
6. **Direct subclasses**
7. [TBoldAbstractListHandle](../BoldAbstractListHandle/TBoldAbstractListHandle.md)
8. `TBoldDerivedHandle`
9. [TBoldExpressionHandle](../BoldExpressionHandle/TBoldExpressionHandle.md)
10. `TBoldFilteredHandle`
11. `TBoldSortedHandle`

## Description

`TBoldRootedHandle` is a subclass of [TBoldElementHandle](../BoldHandles/TBoldElementHandle.md). Everything in its description applies and is for the most part not repeated here.

**Value**

The [value](../BoldHandles/TBoldElementHandle.md#value) of a rooted handle is in some way derived from the Value of RootHandle. The derivation is performed in different ways for different subclasses, and is described in the description for each class.

**Typing**

In the same way as `Value` is derived from `RootHandle.Value`, `StaticBoldType` is derived from `RootHandle.`[StaticBoldType](../BoldHandles/TBoldElementHandle.md#staticboldtype). However, since `RootHandle` may not always be assigned the type can also be specified directly using the [RootTypeName](TBoldRootedHandle.md#roottypename) property.

**Events and Lazy evaluation**

The rooted handles have a "lazy evaluation" scheme. This means that the `Value` will not actually be derived until it is needed. Instead events will be sent it the value has changed (or rather may have changed, since in some cases it will change to the same value).

It is important to note that there is a difference between the fact that the `Value` property changes, and that the contents of the element referred by `Value` changes.

Any event (such as setting `RootHandle`, or `RootHandle.Value` changing) leading to a (possible) change in `Value` will lead to a `beValueIdentityChanged` event being sent to all the subscribers of the handle. However the actual deriving of `Value` will not be done until the property is accessed.

Any event leading to a possible change of the contents of `Value` will lead to the subscribers to Value beeing notified according to their subscriptions.

If a convienient way is needed to subscribe to the value of a handle, without worrying about these two types of subscriptions, a `TBoldPlaceableSubscriber` should be used.

**Subscription**

In general, the handle will subscribe to anything affecting `Value`. In this case `Value` will be automatically reevaluated as needed.

If [Subscribe](TBoldRootedHandle.md#subscribe) is set to `false`, no subscriptions will be placed, and the handle must be manually invalidated by calling [MarkOutOfDate](TBoldRootedHandle.md#markoutofdate)

**Bold Events**

`TBoldRootedHandle` is a subclass of [TBoldSubscribableComponent](../BoldSubscription/TBoldSubscribableComponent.md), and can therefore by subscribed to using `AddSmallSubscription`. A `TBoldRootedHandle` can send the following events:

- `beDestroying`: Sent when the handle is about to be destroyed.
- `beValueIdentityChanged`: Sent when `Value` has changed, i.e. when `Value` points to a new `TBoldElement`. Also sent if anything influencing `StaticBoldType` has changed.

Due to the lazy evaluation, "has changed" has a very specific meaning. It means that the next time the `Value` property is accessed it may return a different value from the previous time. It does not imply that this value has actually been calculated yet.

If several things occur that would change `Value`, but the `Value` property is not accessed in between, only the first will give rise to an event.

| **Note** |
|---|

| The event is not send when the contents of `Value` is changed. This is found out by subscribing to `Value` itself. |
|---|

## Properties

| Name | Summary | Notes |
|---|---|---|
| [Enabled](#enabled) | Toggle the enabled status of the handle |  |
| [InternalRootHandle](#internalroothandle) |  | protected |
| [IsDeriving](#isderiving) |  | protected, read-only |
| [ResultElement](#resultelement) |  | protected, read-only |
| [RootHandle](#roothandle) | The RootHandle property is a reference to another handle. |  |
| [RootTypeName](#roottypename) |  |  |
| [StaticRootType](#staticroottype) | The static type of RootHandle .Value. | read-only |
| [Subscribe](#subscribe) |  |  |

### Enabled

```delphi
property Enabled: Boolean;
```

Setting `Enabled` to `true` will make the handle act according to it's properties.

If `Enabled` is set to `false`, [value](../BoldHandles/TBoldElementHandle.md#value) (and thus [DynamicBoldType](../BoldHandles/TBoldElementHandle.md#dynamicboldtype) will be set to `**nil**`, and all subscriptions will be dropped. This allows disabling handles while still leaving them connected to their root, and with all properties intact.

### InternalRootHandle

```delphi
property InternalRootHandle: TBoldElementHandle;
```

### IsDeriving

```delphi
property IsDeriving: Boolean;
```

### ResultElement

```delphi
property ResultElement: TBoldIndirectElement;
```

### RootHandle

```delphi
property RootHandle: TBoldElementHandle;
```

The [Value](../BoldHandles/TBoldElementHandle.md#value) of a given rooted handle is calculated by applying some calculations to the value of the root. These calculations differ for the various subclasses, and are described in the overview of the class for each handle.

**Bold Events**  
Since changing `RootHandle` indirectly changes `Value` the following events can be sent when setting `RootHandle`:

- `beValueIndentityChanged`: Sent if `RootHandle` is assigned a new value.

If the property is assigned with the same value as it already has the event will not be sent. Also, the event will not be sent if it has previously been sent, and the `Value` property has not been accessed since then.

### RootTypeName

```delphi
property RootTypeName: string;
```

The property is a string, which must be a valid name for a type in [StaticSystemTypeInfo](../BoldHandles/TBoldElementHandle.md#staticsystemtypeinfo). At design time, a property editor is supplied to aid in choosing valid types. Typical values are "String", "Person", "Collection(String)", "Collection(Person)".

`RootTypeName` is used when determining `StaticRootType`.

**Bold events**  
Setting `RootTypeName` gives rise to the following events:

- `beValueIndentityChanged`: Will be send if the value of the property is changed.

Setting the property to its current value will not give an event.

### StaticRootType

```delphi
property StaticRootType: TBoldElementTypeInfo;
```

If `RootHandle` is assigned, it is defined as `RootHandle.StaticBoldType`, otherwise it is determined by applying [RootTypeName](TBoldRootedHandle.md#roottypename) to `StaticSystemTypeInfo`, which in turn is defined by [StaticSystemHandle](../BoldHandles/TBoldNonSystemHandle.md#staticsystemhandle) The property is primarily intended for the internal use of the handles when evaluating `StaticBoldType`.

**Bold events**  
Since `StaticRootType` is a read only property setting it can't directly give rise to events. The following event will however be sent when the value of the property has changed (or rather may have changed, since it may have "changed" to the same value):

- `beValueIndentityChanged`: Will be sent whenever the conditions defining `StaticRootType` have changed.

### Subscribe

```delphi
property Subscribe: Boolean;
```

If `Subscribe` is set to `true`, the handle will subscribe to changes affecting the value of the handle, and ensure that it is always kept current. If `Subscribe` is set to `false` the need to reevaluate must be signaled by calling [MarkOutOfDate](TBoldRootedHandle.md#markoutofdate).

Setting `Subscribe` to `false` will only suppress subscriptions related to calculating `Value`. The internal subscriptions placed by the handle on `Value` itself will still be placed, and thus `Value` will still be set to `**nil**` if the element is destroyed.

**Bold Events**  
Setting subscribe can raise the following events:

- `beValueIndentityChanged`: Send if the value is changed from `false` to `true`.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Create](#create) | Call Create to instantiate an expression-handle at runtime. | override |
| [DefineProperties](#defineproperties) |  | protected, override |
| [DeriveAndSubscribe](#deriveandsubscribe) |  | protected, abstract |
| [Destroy](#destroy) | Destroys the instance | override |
| [EffectiveRootValue](#effectiverootvalue) |  | protected |
| [EffectiveRootValueChanged](#effectiverootvaluechanged) |  | protected, virtual |
| [EnsureCurrent](#ensurecurrent) |  | protected |
| [GetRootHandle](#getroothandle) |  | protected, virtual |
| [GetStaticRootType](#getstaticroottype) |  | protected |
| [GetStaticSystemTypeInfo](#getstaticsystemtypeinfo) |  | protected, override |
| [GetValue](#getvalue) |  | protected, override |
| [IsRootLinkedTo](#isrootlinkedto) |  |  |
| [Loaded](#loaded) |  | protected, override |
| [MarkOutOfDate](#markoutofdate) |  | virtual |
| [MarkSubscriptionOutOfDate](#marksubscriptionoutofdate) |  | protected |
| [RefersToComponent](#referstocomponent) |  | override |
| [SetEnabled](#setenabled) |  | protected, virtual |
| [SetRootHandle](#setroothandle) |  | protected, virtual |
| [SetSubscribe](#setsubscribe) |  | protected, virtual |
| [SubscribeToValue](#subscribetovalue) |  | protected |
| [ValueIdentityChanged](#valueidentitychanged) |  | protected |

### Create

```delphi
constructor Create(Owner: TComponent); override; See also Ancestor Method
```

Components placed in forms or data modules at design time are created automatically.

### DefineProperties

```delphi
procedure DefineProperties(Filer: TFiler); override;
```

### DeriveAndSubscribe

```delphi
procedure DeriveAndSubscribe(DerivedObject: TObject; Subscriber: TBoldSubscriber); virtual; abstract;
```

### Destroy

```delphi
destructor Destroy; override; See also Ancestor Method
```

**Bold events**  
Calling `Destroy` can result in the following events:

- `beDestroying`: Sent when the handle is about to be destroyed, i..e before any part of the destruction has been performed.

### EffectiveRootValue

```delphi
function EffectiveRootValue: TBoldElement;
```

### EffectiveRootValueChanged

```delphi
procedure EffectiveRootValueChanged; virtual;
```

### EnsureCurrent

```delphi
procedure EnsureCurrent;
```

### GetRootHandle

```delphi
function GetRootHandle: TBoldElementHandle; virtual;
```

### GetStaticRootType

```delphi
function GetStaticRootType: TBoldElementTypeInfo;
```

### GetStaticSystemTypeInfo

```delphi
function GetStaticSystemTypeInfo: TBoldSystemTypeInfo; override; See also Ancestor Method
```

### GetValue

```delphi
function GetValue: TBoldElement; override; See also Ancestor Method
```

### IsRootLinkedTo

```delphi
function IsRootLinkedTo(Handle: TBoldElementHandle): Boolean;
```

The function returns `true` if the handle is directly or indirectly linked the the Handle parameter via the [RootHandle](TBoldRootedHandle.md#roothandle) property.

### Loaded

```delphi
procedure Loaded; override;
```

### MarkOutOfDate

```delphi
procedure MarkOutOfDate; virtual;
```

This method is used for handles that don't [subscribe](TBoldRootedHandle.md#subscribe) to their conditions. Calling `markOutOfDate` will lead to full reevaluation of [value](../BoldHandles/TBoldElementHandle.md#value) the next time the `Value` property is accessed. It will also lead to subscribers to the handle recieveing a `beValueIdentityChanged`.

**Bold Events**  
Calling `MarkOutOfdate` raises the following event:

- `beValueIndentityChanged`: Sent when the method is called.

The event will not be sent if it has previously been sent, and the `Value` property has not been accessed since then.

### MarkSubscriptionOutOfDate

```delphi
procedure MarkSubscriptionOutOfDate;
```

### RefersToComponent

```delphi
function RefersToComponent(Component: TBoldSubscribableComponent): Boolean; override; See also Ancestor Method
```

### SetEnabled

```delphi
procedure SetEnabled(Value: Boolean); virtual;
```

### SetRootHandle

```delphi
procedure SetRootHandle(const Value: TBoldElementHandle); virtual;
```

### SetSubscribe

```delphi
procedure SetSubscribe(Value: Boolean); virtual;
```

### SubscribeToValue

```delphi
procedure SubscribeToValue;
```

### ValueIdentityChanged

```delphi
procedure ValueIdentityChanged;
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
