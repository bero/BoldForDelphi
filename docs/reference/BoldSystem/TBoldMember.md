# TBoldMember

The members of Bold objects are of the type TBoldMember.

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldMember = class(TBoldDomainElement)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldFlaggedObject`
4. [TBoldSubscribableObject](../BoldSubscription/TBoldSubscribableObject.md)
5. [TBoldElement](../BoldElements/TBoldElement.md)
6. [TBoldDomainElement](../BoldDomainElement/TBoldDomainElement.md)
7. TBoldMember
8. **Direct subclasses**
9. [TBoldAttribute](TBoldAttribute.md)
10. [TBoldList](TBoldList.md)
11. [TBoldObjectReference](TBoldObjectReference.md)

## Description

The members of Bold objects are of the type `TBoldMember`. But a `TBoldMember` doesn't have to belong to a Bold object. It can be, and is often, used as a standalone variable or constant. In that case, its [BoldMemberRtInfo](TBoldMember.md#boldmemberrtinfo) and [OwningObject](TBoldMember.md#owningobject) properties are `**nil**`.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [AsIBoldValue](#asiboldvalue) | The IBoldValue interface. | read-only |
| [BoldMemberRTInfo](#boldmemberrtinfo) | The model information for the member | read-only |
| [BoldPersistenceState](#boldpersistencestate) | The relationship between the member's value in memory and in the persistent storage. |  |
| [BoldSystem](#boldsystem) | The BoldSystem that the member belongs to. | read-only |
| [Derived](#derived) | Indicates if the member is derived. | read-only |
| [Deriver](#deriver) | There is normally no need for the programmer to access the deriver directly. | read-only |
| [IsPartOfSystem](#ispartofsystem) | Indicates if the member is part of a BoldSystem. | read-only |
| [OldValue](#oldvalue) | Bold-internal | read-only |
| [OwningObject](#owningobject) | The Bold object the member is a member of. | read-only |
| [Touched](#touched) | Bold-internal | read-only |

### AsIBoldValue

```delphi
property AsIBoldValue[Mode:TBoldDomainElementProxyMode]: IBoldValue;
```

Use this property to get the IBoldValue for the member. This is used internally.

### BoldMemberRTInfo

```delphi
property BoldMemberRTInfo: TBoldMemberRTInfo;
```

If the `TBoldMember` is a member of a Bold object, the `BoldMemberRTInfo` property will be assigned. It contains the model information for the member.

### BoldPersistenceState

```delphi
property BoldPersistenceState: TBoldValuePersistenceState;
```

The persistence state of a Bold member reflects the relationship between its value in memory and in the persistent storage. It tells us which of the two is the most recent.

If the state of a member is `bvpsInvalid` it means that the value in memory is not correct. It will be fetched automatically when the member is accessed. After the value has been fetched the state will become `bvpsCurrent`, meaning that the value in memory is the same as the value in the persistent storage. If the value of the member is changed in memory the state will change to `bvpsModified`, implying that the new value should be written to the persistent storage. Finally, the `bvpsTransient` state tells us that the member only exists in memory, either because the object it belongs to is transient, or because the member is marked as transient in the model.

Calling [Invalidate](TBoldMember.md#invalidate) makes a member 'forget' that it has been fetched. This changes its state from `bvpsCurrent` to `bvpsInvalid`. It is not allowed to invalidate members in `bvpsModified` state. Similarly, [Discard](TBoldMember.md#discard) makes a member 'forget' that it has been modified. It changes it from `bvpsModified` to `bvpsInvalid`.

Finally, members can change state from `bvpsTransient` to `bvpsModified`. This happens automatically if the object the member belongs to becomes persistent. See [TBoldObject.BoldMakePersistent](TBoldObject.md#boldmakepersistent).

Before a member's value is modified in memory the method `CanModify` is called to see if modifying the member is allowed. `CanModify`, among other things, checks the virtual [MayModify](TBoldMember.md#maymodify) method and the `bqMayModify` query-event. If the modification is allowed the virtual [PrepareModify](TBoldMember.md#preparemodify) method is called and the event `bePrepareModify` is sent. After the value has been changed the virtual method [CompleteModify](TBoldMember.md#completemodify) is called and the event `beCompleteModify` is sent.

### BoldSystem

```delphi
property BoldSystem: TBoldSystem;
```

For a member that is part of a Bold object, this is the system that the object belongs to. For standalone members, it is `**nil**`.

### Derived

```delphi
property Derived: Boolean;
```

Indicates if the member is derived.

### Deriver

```delphi
property Deriver: TBoldEventPluggedDeriver;
```

There is normally no need for the programmer to access the deriver directly.

### IsPartOfSystem

```delphi
property IsPartOfSystem: Boolean;
```

If `IsPartOfSystem` is `true`, the [BoldSystem](TBoldMember.md#boldsystem) property is valid. If not, accessing [BoldSystem](TBoldMember.md#boldsystem) will raise an exception.

### OldValue

```delphi
property OldValue: IBoldValue;
```

Bold-internal.

### OwningObject

```delphi
property OwningObject: TBoldObject;
```

Returns the Bold object the member is a member of. If the member is not member of a Bold object, the function returns `**nil**`.

### Touched

```delphi
property Touched: Boolean;
```

Bold-internal

## Methods

| Name | Summary | Notes |
|---|---|---|
| [AtTime](#attime) |  | virtual |
| [CanModify](#canmodify) | If the member is allowed to be modified |  |
| [CanRead](#canread) | Indicates if the value of the member may be read |  |
| [CanUpdate](#canupdate) | If the member is allowed to be updated |  |
| [Changed](#changed) | Called internally when the value of the member has changed. | protected |
| [Clone](#clone) | Makes a copy of the member. |  |
| [CloneIfPossible](#cloneifpossible) | This will create a clone of the member and copy the contents. | protected, override |
| [CompleteModify](#completemodify) | Occurs directly after the member is modified. | protected, virtual |
| [CompleteUpdate](#completeupdate) | Virtual method that is called after a member has been updated in the persistent storage | protected, virtual |
| [Create](#create) | Creates an element that is not part of a Bold object. |  |
| [CreateWithTypeInfo](#createwithtypeinfo) | Constructor with Bold type information |  |
| [Destroy](#destroy) | Destructor | override |
| [Discard](#discard) | Discards changes in memory |  |
| [DoStartModify](#dostartmodify) | Bold-internal method that performs preparations needed before modifying the value of a member. | protected |
| [EndModify](#endmodify) | Called by member subclasses after the value has been modified. | protected |
| [EnsureContentsCurrent](#ensurecontentscurrent) | If the value of the member is not current, make it current. |  |
| [FailModify](#failmodify) | Called by member subclasses if a modification fails. | protected |
| [FreeContent](#freecontent) | Releases the memory used for the data value. | protected, virtual |
| [GetAsList](#getaslist) | Overrides TBoldElement.GetAsList | override |
| [GetAsValue](#getasvalue) | Bold-internal | override |
| [GetBoldDirty](#getbolddirty) | Overrides TBoldDomainElement.GetBoldDirty | protected, override |
| [GetBoldPersistenceState](#getboldpersistencestate) | Get-method for the BoldPersistenceState property | protected |
| [GetBoldType](#getboldtype) | Overrides TBoldElement.GetBoldType | protected, override |
| [GetDisplayName](#getdisplayname) | Overrides TBoldElement.GetDisplayName | protected, override |
| [GetEvaluator](#getevaluator) | Overrides TBoldElement.GetEvaluator | protected, override |
| [GetStreamName](#getstreamname) | Name that identifies the type of the object for the streaming mechanisms. | protected, virtual |
| [InitializeMember](#initializemember) | Bold-internal | protected, virtual |
| [Invalidate](#invalidate) |  |  |
| [IsEqualToValue](#isequaltovalue) | Similar to IsEqual, but compares with a IBoldValue. | virtual |
| [MayModify](#maymodify) | Override this function and return false to prohibit the modify state transition. | protected, virtual |
| [MayUpdate](#mayupdate) | Override this function and return false to prohibit the update state transition. | protected, virtual |
| [MemberHasSubscribers](#memberhassubscribers) | True if there is anything subscribing to the member. |  |
| [ObserverMayModify](#observermaymodify) |  | override |
| [PreChange](#prechange) | Called by subclasses before the data value changes. | protected |
| [PrepareModify](#preparemodify) | Override this method to add behaviour before a member is modified. | protected, virtual |
| [PrepareUpdate](#prepareupdate) | Override this method to add behaviour before a member is saved to persistent storage. | protected, virtual |
| [ProxyClass](#proxyclass) | The Delphi class used by ProxyInterface | protected, abstract |
| [ProxyInterface](#proxyinterface) | Overrides TBoldElement.ProxyInterface | override |
| [Refetch](#refetch) | Fetches the value from persistent storage. |  |
| [RetrieveProxyInterface](#retrieveproxyinterface) | Internal. Called by subclasses. | protected |
| [SetBoldPersistenceState](#setboldpersistencestate) | Bold-internal. Sets the persistence state. | protected |
| [StartModify](#startmodify) |  | protected, virtual |
| [StateError](#stateerror) | Overrides TBoldDomainElement.StateError | protected, override |
| [StoreInUndo](#storeinundo) | Bold-internal |  |

### AtTime

```delphi
function AtTime(Time: TBoldTimestampType): TBoldMember; virtual;
```

Returns the member as it looked at the time point identified by the Time time stamp.

| **Note** |
|---|

| This feature is only available in the Object Versioning Extension to Bold for Delphi. |
|---|

### CanModify

```delphi
function CanModify: Boolean;
```

Returns `True` if the member is allowed to be modified. A programmer can disallow modify either by overriding the virtual `MayModify` function or subscribing to the `bqMayModify` query.

### CanRead

```delphi
function CanRead(Subscriber: TBoldSubscriber): Boolean;
```

Returns `True` if the value of the member may be read. A programmer can disallow reading a member by subscribing to the `bqMayRead` query. If a `Subscriber` is passed to the function, subscriptions will be placed to notify when the result of `MayRead` may have changed.

### CanUpdate

```delphi
function CanUpdate: Boolean;
```

Returns `True` if the member is allowed to be updated. A programmer can disallow update either by overriding the virtual `MayUpdate` function or subscribing to the `bqMayUpdate` query.

### Changed

```delphi
procedure Changed(Event: TBoldEvent; const Args: array of const);
```

This method shall be called whenever the contents (technical value) of the member has changed. The `Event` and `Args` parameters should contain the values corresponding to a call to SendExtendedEvent. The event will be sent unless [BoldPersistenceState](TBoldMember.md#boldpersistencestate) is `bvpsInvalid`.

It is important that all subclasses to `TBoldMember`, i.e. custom attributes call this method.

### Clone

```delphi
function Clone: TBoldMember;
```

Makes a copy of the member. The resulting member will be of the same type and will contain the same value as the original.

### CloneIfPossible

```delphi
function CloneIfPossible: TBoldElement; override; See also Ancestor Method
```

This will create a clone of the member and copy the contents.

### CompleteModify

```delphi
procedure CompleteModify; virtual;
```

Override this method to add behaviour after a member is modified. The default implementation is empty.

**See Also**

- TBoldValuePersistenceState

### CompleteUpdate

```delphi
procedure CompleteUpdate; virtual;
```

Override this method to add behaviour after a member is updated in the persistent storage. The default implementation is empty.

### Create

```delphi
constructor Create;
```

Contructor for member. Will create a member that is freestanding from any Bold object or system.

### CreateWithTypeInfo

```delphi
constructor CreateWithTypeInfo(ElementTypeInfo: TBoldElementTypeInfo);
```

As `TBoldMember` is an abstract class, `CreateWithTypeInfo` should not be called directly on `TBoldMember`, but on one of its concrete subclasses. Use this constructor instead of `Create` to explicitly specify the type info for the member. This is useful for creating members with types that are not in the default BoldSystem.

### Destroy

```delphi
destructor Destroy; override; See also Ancestor Method
```

Destructor

### Discard

```delphi
procedure Discard;
```

If any changes have been made to the member since it was fetched from persistent storage, or updated to persistent storage, these changes will be lost. This means that a member with [BoldPersistenceState](TBoldMember.md#boldpersistencestate) = `bvpsModified` will have its persistence state changed to `bvpsInvalid`.

### DoStartModify

```delphi
procedure DoStartModify;
```

Bold-internal method that performs preparations needed before modifying the value of a member.

### EndModify

```delphi
procedure EndModify;
```

Triggers the EndModify state transition. EndModify will be called automatically when a member is modified.

**See Also**

- TBoldValuePersistenceState

### EnsureContentsCurrent

```delphi
procedure EnsureContentsCurrent;
```

If the value of the member is not current, make it current. A persistent member will be fetched, and a derived attribute will be derived.

### FailModify

```delphi
procedure FailModify;
```

Triggers the FailModify state transition. `FailModify` will be called automatically when a modify operation fails.

**See Also**

- TBoldValuePersistenceState

### FreeContent

```delphi
procedure FreeContent; virtual;
```

This method is implemented by memory consuming subclasses, such as blob attributes and multilinks, to release memory. It will be called when the value is not needed any more.

### GetAsList

```delphi
procedure GetAsList(ResultList: TBoldIndirectElement); override; See also Ancestor Method
```

Sets `ResultList` to point to a [TBoldMemberList](TBoldMemberList.md) containing the member.

### GetAsValue

```delphi
procedure GetAsValue(ResultElement: TBoldIndirectElement); override; See also Ancestor Method
```

Bold-internal. Overrides [GetAsValue](../BoldElements/TBoldElement.md#getasvalue).

### GetBoldDirty

```delphi
function GetBoldDirty: Boolean; override; See also Ancestor Method
```

Overrides [GetBoldDirty](../BoldDomainElement/TBoldDomainElement.md#getbolddirty). A member is dirty if its persistence state is bvpsModified; if its value has been modified in memory.

### GetBoldPersistenceState

```delphi
function GetBoldPersistenceState: TBoldValuePersistenceState;
```

Get-method for the [BoldPersistenceState](TBoldMember.md#boldpersistencestate) property

### GetBoldType

```delphi
function GetBoldType: TBoldElementTypeInfo; override; See also Ancestor Method
```

Overrides `TBoldElement.GetBoldType`

### GetDisplayName

```delphi
function GetDisplayName: String; override; See also Ancestor Method
```

Overrides GetDisplayName. The display name for a member that is part of a Bold object is "<classname>.<membername>".

### GetEvaluator

```delphi
function GetEvaluator: TBoldEvaluator; override; See also Ancestor Method
```

Overrides [GetEvaluator](../BoldElements/TBoldElement.md#getevaluator). If the member belongs to a system, that system's evaluator is used.

### GetStreamName

```delphi
function GetStreamName: string; virtual;
```

Name that identifies the type of the object for the streaming mechanisms.

### InitializeMember

```delphi
procedure InitializeMember(AOwningElement: TBoldDomainElement; ElementTypeInfo: TBoldElementTypeInfo); virtual;
```

Bold-internal

### Invalidate

```delphi
procedure Invalidate;
```

Call this method if you beleive that the value of the member may have changed in the persistent storage. This will cause the member to be refetched the next time its value is accessed. Members with `BoldValuePersistenceState = bvpsCurrent` will have their persistence state changed to `bvpsInvalid`. Calling `Invalidate` on modified or transient members will cause an exception to be raised.

### IsEqualToValue

```delphi
function IsEqualToValue(Value: IBoldValue): Boolean; virtual;
```

Similar to [IsEqual](../BoldElements/TBoldElement.md#isequal), but compares with an IBoldValue.

### MayModify

```delphi
function MayModify: Boolean; virtual;
```

Override this function and return `false` to prohibit the modify state transition. See also [CanModify](TBoldMember.md#canmodify) and [StartModify](TBoldMember.md#startmodify).

### MayUpdate

```delphi
function MayUpdate: Boolean; virtual;
```

Override this function and return `false` to prohibit the update state transition. See also [CanUpdate](TBoldMember.md#canupdate).

### MemberHasSubscribers

```delphi
function MemberHasSubscribers: Boolean;
```

`True` if there is anything subscribing to the member. See Subscription.

### ObserverMayModify

```delphi
function ObserverMayModify(Observer: TObject): Boolean; override; See also Ancestor Method
```

Overrides [ObserverMayModify](../BoldElements/TBoldElement.md#observermaymodify)

### PreChange

```delphi
procedure PreChange;
```

Called by subclasses before the data value changes.

### PrepareModify

```delphi
procedure PrepareModify; virtual;
```

Override this method to add behaviour before a member is modified. The default implementation is empty.

**See Also**

- TBoldValuePersistenceState

### PrepareUpdate

```delphi
procedure PrepareUpdate; virtual;
```

Override this method to add behaviour before a member is saved to persistent storage. The default implementation is empty.

**See Also**

- TBoldValuePersistenceState

### ProxyClass

```delphi
function ProxyClass: TBoldMember_ProxyClass; virtual; abstract;
```

The Delphi class used by [ProxyInterface](TBoldMember.md#proxyinterface). Implemented by subclasses.

### ProxyInterface

```delphi
function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override; See also Ancestor Method
```

Overrides ProxyInterface.

### Refetch

```delphi
procedure Refetch;
```

Fetches the value from persistent storage regardless of whether the member is already fetched. This does not discard the value of a modified member. Modified members are not affected by a call to `Refetch`.

### RetrieveProxyInterface

```delphi
function RetrieveProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj; const InterfaceName: string): Boolean;
```

Internal. This method is called by subclasses to implement ProxyInterfaces.

### SetBoldPersistenceState

```delphi
procedure SetBoldPersistenceState(Value: TBoldValuePersistenceState);
```

Bold-internal. Sets the persistence state. Normally there is no need to call this method.

### StartModify

```delphi
function StartModify: Boolean; virtual;
```

Triggers the StartModify state transition. `StartModify` will be called automatically before a member is updated. The result will be `True` if the state transition was succesful. `CanModify` will return the same value as `StartModify` without performing the state transition.

**See Also**

- TBoldValuePersistenceState

### StateError

```delphi
procedure StateError(S: String); override; See also Ancestor Method
```

Overrides [TBoldDomainElement](../BoldDomainElement/TBoldDomainElement.md)

### StoreInUndo

```delphi
function StoreInUndo: Boolean;
```

Bold-internal. Used by the undo-mechanism.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
