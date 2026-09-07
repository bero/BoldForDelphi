# TBoldObject

TBoldObject is the representation of Bold objects.

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldObject = class(TBoldDomainElement)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldFlaggedObject`
4. [TBoldSubscribableObject](../BoldSubscription/TBoldSubscribableObject.md)
5. [TBoldElement](../BoldElements/TBoldElement.md)
6. [TBoldDomainElement](../BoldDomainElement/TBoldDomainElement.md)
7. TBoldObject
8. **Direct subclasses**
9. `TBusinessClassesRoot`

## Description

`TBoldObject` is the representation of Bold objects. The business classes in the generated code will all be subclasses of this class. The business objects in a Bold application are instances of this class, or one of its subclasses.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [AsIBoldObjectContents](#asiboldobjectcontents) | This property gives the ValueSpace view of an object. | read-only |
| [BoldClassTypeInfo](#boldclasstypeinfo) | The class type info holds the model information for objects of this type. | read-only |
| [BoldExistenceState](#boldexistencestate) | The existence state of a Bold object tells us if the conceptual object exists or not. | read-only |
| [BoldMemberAssigned](#boldmemberassigned) | If a specific member of the object has been created yet. | read-only |
| [BoldMemberByExpressionName](#boldmemberbyexpressionname) | The member with a given expression name. | read-only |
| [BoldMemberCount](#boldmembercount) | The total number of members of the object | read-only |
| [BoldMemberIndexByExpressionName](#boldmemberindexbyexpressionname) | The index of the member with a given expression name. | read-only |
| [BoldMembers](#boldmembers) | The members of a Bold object are the objects that represent the attributes and roles of the object. | read-only |
| [BoldObjectExists](#boldobjectexists) | If the object exists conceptually | read-only |
| [BoldObjectIsDeleted](#boldobjectisdeleted) | If the object has been deleted | read-only |
| [BoldObjectIsNew](#boldobjectisnew) | If the object has been created, but not yet saved. | read-only |
| [BoldObjectLocator](#boldobjectlocator) | The object's locator | read-only |
| [BoldPersistenceState](#boldpersistencestate) | Reflects the relationship between the value in memory and in persistent storage. | read-only |
| [BoldSystem](#boldsystem) | The system the object belongs to. | read-only |
| [BoldTime](#boldtime) | Returns the time stamp associated with this version of the Bold object. | read-only |
| [ObjectHasSubscribers](#objecthassubscribers) | True if there is anything subscribing to the object. | read-only |
| [Touched](#touched) | Bold-internal | read-only |

### AsIBoldObjectContents

```delphi
property AsIBoldObjectContents[Mode:TBoldDomainElementProxyMode]: IBoldObjectContents;
```

This property gives the `ValueSpace` view of an object.

### BoldClassTypeInfo

```delphi
property BoldClassTypeInfo: TBoldClassTypeInfo;
```

The class type info holds the model information for objects of this type.

### BoldExistenceState

```delphi
property BoldExistenceState: TBoldExistenceState;
```

The existence state of a Bold object tells us if the conceptual object exists or not.

- If the value is `besNotCreated` then the object has not yet been created. An object is typically only in this state during the early phases of its creation.
- The value `besExisting` means that the conceptual object exists.
- The value `besDeleted` means that the object has been deleted. Thus, the object conceptually does not exist anymore. Objects with this state are kept in memory as a reminder to also delete them from the database.

When the existence state changes from `besExisting` to either `besNotCreated` or

| **Note** |
|---|

| besDeleted |
|---|

, the event `beObjectDeleted` is sent. When the state changes from either `besNotCreated` or `besDeleted` to `besExisting`, the event `beObjectCreated` is sent.

### BoldMemberAssigned

```delphi
property BoldMemberAssigned[index:Integer]: Boolean;
```

For reasons of conserving memory, the individual members of a Bold object (instances of `TBoldMember`) are not created until the first time they are needed. This property can be used to check if a specific member has been created, so as not to unnecessarily create it.

### BoldMemberByExpressionName

```delphi
property BoldMemberByExpressionName[constname:string]: TBoldMember;
```

The member with a given expression name.

### BoldMemberCount

```delphi
property BoldMemberCount: Integer;
```

The total number of members of the object. Can be used to loop over the [BoldMembers](TBoldObject.md#boldmembers) property.

### BoldMemberIndexByExpressionName

```delphi
property BoldMemberIndexByExpressionName[constname:string]: Integer;
```

The index of the member with a given expression name. `-1` if there is no member with that name.

### BoldMembers

```delphi
property BoldMembers[index:Integer]: TBoldMember;
```

The members of a Bold object are the objects that represent the attributes and roles of the object. This property allows access to these objects using their index.

### BoldObjectExists

```delphi
property BoldObjectExists: Boolean;
```

This property corresponds to testing `BoldExistenceState = besExisting`, and will be be `true` if the object conceptually exists, and `false` if it has been deleted (and for a short time during the creation-phase of the object).

### BoldObjectIsDeleted

```delphi
property BoldObjectIsDeleted: Boolean;
```

This property will be true for a Bold object after [Delete](TBoldObject.md#delete) has been called.

### BoldObjectIsNew

```delphi
property BoldObjectIsNew: Boolean;
```

If the object has been created, but not yet saved.

### BoldObjectLocator

```delphi
property BoldObjectLocator: TBoldObjectLocator;
```

The object's locator

### BoldPersistenceState

```delphi
property BoldPersistenceState: TBoldValuePersistenceState;
```

The persistence state of a Bold object reflects the relationship between its value in memory and in the persistent storage. It tells us which of the two is the most recent. The value of a Bold object is concidered to be its existence state.

If the state of an object is `bvpsInvalid` it means that the value in memory is not correct. This means that the object is being fetched. After the object has been fetched the state will become `bvpsCurrent`. If the object is deleted in memory the state will change to `bvpsModified`, implying that the object should be deleted from the persistent storage. When a new object is created it also receives the `bvpsModified` state. Finally, the `bvpsTransient` state tells us that the object only exists in memory.

Calling [Invalidate](TBoldObject.md#invalidate) on an object only invalidates the members of the object, and does not set the object's persistence state to invalid.

[Discard](TBoldObject.md#discard) makes an object 'forget' that it has been created or deleted. It changes its state from `bvpsModified` to `bvpsInvalid`. It also discards the members of the object.

Objects can change state from `bvpsTransient` to `bvpsModified`. This happens when the method `BoldMakePersistent` is called, assuming that the object's class is persistent.

After an object is created the virtual method [CompleteCreate](TBoldObject.md#completecreate) is called. Override this to add initialization code. The event `beCompleteModify` is also sent at this time.

Before an object is deleted the method [CanDelete](TBoldObject.md#candelete) is checked. This, among other things, calls the virtual [MayDelete](TBoldObject.md#maydelete) method and the `bqMayDelete` query-event. If the delete operation is allowed the virtual method [PrepareDelete](TBoldObject.md#preparedelete) is called before the existence state is changed. The event `bePrepareModify` is also sent at this time.

### BoldSystem

```delphi
property BoldSystem: TBoldSystem;
```

The system the object belongs to.

### BoldTime

```delphi
property BoldTime: TBoldTimestampType;
```

Each version of a Bold object is represented in memory by a separate instance of TBoldObject (or subclass). The `BoldTime` property returns the time stamp for this version.

| **Note** |
|---|

| This feature is only available in the Object Versioning Extension. |
|---|

### ObjectHasSubscribers

```delphi
property ObjectHasSubscribers: Boolean;
```

`True` if there is anything subscribing to the object.

**See Also**

- Subscriptions

### Touched

```delphi
property Touched: Boolean;
```

Bold-internal

## Methods

| Name | Summary | Notes |
|---|---|---|
| [AtTime](#attime) |  |  |
| [BoldMakePersistent](#boldmakepersistent) |  |  |
| [CanDelete](#candelete) | True if the object is allowed to be deleted. |  |
| [CanUpdate](#canupdate) | True if the object is allowed to be updated. |  |
| [CheckLinks](#checklinks) | Checks that there are no associations to the object. |  |
| [ClearTouched](#cleartouched) | Bold-internal |  |
| [CompleteCreate](#completecreate) | Override this method to add behaviour after an object is created. | protected, virtual |
| [CompleteUpdate](#completeupdate) | Virtual method that is called after an object has been updated in the persistent storage | protected, virtual |
| [Create](#create) | Creates a Bold object. |  |
| [DefaultSubscribe](#defaultsubscribe) | Subscribes to the default events on the Bold object. | override |
| [Delete](#delete) | Deletes the Bold object. |  |
| [Destroy](#destroy) | Destructor | override |
| [Discard](#discard) | Discards all changes made in memory, that have not been written to persistent storage yet. |  |
| [GetAsList](#getaslist) | Overrides TBoldElement.GetAsList | override |
| [GetBoldDirty](#getbolddirty) | Overrides TBoldDomainElement.GetBoldDirty | protected, override |
| [GetBoldType](#getboldtype) | Overrides TBoldElement.GetBoldType. | protected, override |
| [GetDeriveMethodForMember](#getderivemethodformember) | Internal. Sets up derived attributes. | protected, virtual |
| [GetDisplayName](#getdisplayname) | Overrides TBoldElement.GetDisplayName. | protected, override |
| [GetEvaluator](#getevaluator) | Overrides TBoldElement.GetEvaluator. | protected, override |
| [GetReverseDeriveMethodForMember](#getreversederivemethodformember) | Internal. Sets up derived attributes. | protected, virtual |
| [GetStringRepresentation](#getstringrepresentation) | Overrides TBolfElement.GetStringRepresentation | protected, override |
| [InternalCreateNewWithClassAndSystem](#internalcreatenewwithclassandsystem) | Bold-internal. |  |
| [Invalidate](#invalidate) | Invalidates the members of the object. |  |
| [IsEqualAs](#isequalas) | Overrides TBoldElement.IsEqualAs | override |
| [MarkObjectDirty](#markobjectdirty) | Forces the object to become dirty. |  |
| [MayDelete](#maydelete) | User overrideable function. Return false to prohibit deleting the object. | protected, virtual |
| [MayUpdate](#mayupdate) | Override this function and return false to prohibit the update state transition. | protected, virtual |
| [PrepareDelete](#preparedelete) | Override this method to add behaviour before an object is deleted. | protected, virtual |
| [PrepareUpdate](#prepareupdate) | Override this method to add behaviour before an object is updated. | protected, virtual |
| [ProxyInterface](#proxyinterface) | Overrides TBoldElement.ProxyInterface | override |
| [ReceiveEventFromOwned](#receiveeventfromowned) | Overrides TBoldDomainElement.ReceiveEventFromOwned | override |
| [ReRead](#reread) | The object will be fetched even if its persistence state is current. |  |
| [StateError](#stateerror) | Overrides TBoldDomainElement.StateError. | protected, override |
| [SubscribeToStringRepresentation](#subscribetostringrepresentation) | Overrides TBoldElement.SubscribeToStringRepresentation. | override |
| [ToBeRemovedClassAccessed](#toberemovedclassaccessed) | Called when a class with EvolutionState ToBeRemoved is accessed. | protected, virtual |
| [ToBeRemovedMemberAccessed](#toberemovedmemberaccessed) | Called when a member with EvolutionState ToBeRemoved is accessed. | protected, virtual |
| [ToBeRemovedMemberModified](#toberemovedmembermodified) | Called when a member with EvolutionState ToBeRemoved is modified. | protected, virtual |
| [UnLinkAll](#unlinkall) | Clear all associations to the object. |  |
| [ValidateMember](#validatemember) | Internal convenience method | protected |

### AtTime

```delphi
function AtTime(Time: TBoldTimestampType): TBoldObject;
```

Returns the object as it looked at the time point identified by the Time time stamp.

| **Note** |
|---|

| This feature is only available in the Object Versioning Extension to Bold for Delphi. |
|---|

### BoldMakePersistent

```delphi
procedure BoldMakePersistent;
```

If you have created a transient instance of a persistent class, `BoldMakePersistent` will make that instance persistent. Note that this will not actually save the Bold object to the persistent storage, but rather mark it as a newly created instance.

Transient instances of persistent classes can be created by passing `False` as the second parameter to [create](TBoldObject.md#create).

### CanDelete

```delphi
function CanDelete: Boolean;
```

Returns `True` if the object is allowed to be deleted. A programmer can disallow deleting either by overriding the virtual `MayDelete` function or subscribing to the `bqMayDelete` query.

### CanUpdate

```delphi
function CanUpdate: Boolean;
```

Returns `True` if the object is allowed to be updated. A programmer can disallow updating either by overriding the virtual `MayUpdate` function or subscribing to the `bqMayUpdate` query.

### CheckLinks

```delphi
function CheckLinks(index: Integer): Boolean;
```

Checks that there are no associations to the object, except by the role with member index `**Index**`.

### ClearTouched

```delphi
procedure ClearTouched;
```

Bold-internal

### CompleteCreate

```delphi
procedure CompleteCreate; virtual;
```

Override this method to add behaviour after an object is created, such as setting default values for attributes. The default implementation is empty.

**See Also**

- TBoldValuePersistenceState

### CompleteUpdate

```delphi
procedure CompleteUpdate; virtual;
```

Override this method to add behaviour after an object is updated in the persistent storage. The default implementation is empty.

### Create

```delphi
constructor Create(AOwningElement: TBoldDomainElement; Persistent: Boolean = True);
```

Creates a Bold object. This constructor can not be called on `TBoldObject`, but should be called on a subclass. The `OwningElement` should be a [TBoldSystem](TBoldSystem.md). If `**nil**`, the object will belong to the default system.

Good practice is to specify the system explicitly.

### DefaultSubscribe

```delphi
procedure DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override; See also Ancestor Method
```

Subscribes to the default events on the Bold object. There are no default events for a `TBoldObject`, so, in reality, no subscriptions are placed.

**See Also**

- Subscriptions

### Delete

```delphi
procedure Delete;
```

Deletes the Bold object. If the object is transient or has not been written to the persistent storage the Delphi object is destroyed. Otherwise, the object is marked for deletion and will be deleted from the persistent storage and memory when [UpdateDatabase](TBoldSystem.md#updatedatabase) is called on the owning system.

Delete triggers a change in the persistence state of the object.

Before a Bold object is deleted it will try to clear all associations. The clearing of associations will depend on the setting of the `DeleteAction` of the association. Details can be found in the Model Information topic (Association Ends/DeleteAction).

| **Note** |
|---|

| When an object is deleted, it is not always immediately destroyed. If the object is persistent, the object will be kept by the system until it the delete is saved to the database. It is however not advised to hold any pointers to a deleted objects unless for specific technical reasons. The obejct will send the event beObjectDeleted when it is being deleted, and the event beDestroying when it is destroyed |
|---|

**See Also**

- TBoldValuePersistenceState

### Destroy

```delphi
destructor Destroy; override; See also Ancestor Method
```

Do not call `TBoldObject.Destroy` directly. To delete a Bold object, call [Delete](TBoldObject.md#delete). To remove the object from memory without deleting it in the persistent storage, call [UnloadBoldObject](TBoldObjectLocator.md#unloadboldobject) on the locator of the object.

### Discard

```delphi
procedure Discard;
```

All changes made to the object since it was fetched from, or updated to, the persistent storage will be lost. All modified members will be discarded. If the object is a newly created object or a transient object, it will be deleted, and thus destroyed in memory. If the object is a deleted object it will be undeleted and its persistence state will change to `bvpsCurrent`.

### GetAsList

```delphi
procedure GetAsList(ResultList: TBoldIndirectElement); override; See also Ancestor Method
```

Sets ResultList to point to a [TBoldObjectList](TBoldObjectList.md) containing the object.

### GetBoldDirty

```delphi
function GetBoldDirty: Boolean; override; See also Ancestor Method
```

A `TBoldObject` is considered dirty if one of its members is dirty, or if the object itself has persistence state bvpsModified. This is the case for a newly created or deleted object.

### GetBoldType

```delphi
function GetBoldType: TBoldElementTypeInfo; override; See also Ancestor Method
```

Overrides [GetBoldType](../BoldElements/TBoldElement.md#getboldtype). The bold type of a `TBoldObject` is the same as the [BoldClassTypeInfo](TBoldObject.md#boldclasstypeinfo).

### GetDeriveMethodForMember

```delphi
function GetDeriveMethodForMember(Member: TBoldMember): TBoldDeriveAndResubscribe; virtual;
```

The implementation of this method will be automatically generated when generating code for a class that has derived attributes.

### GetDisplayName

```delphi
function GetDisplayName: String; override; See also Ancestor Method
```

Overrides GetDisplayName. The display name for a Bold object is the name of the class.

### GetEvaluator

```delphi
function GetEvaluator: TBoldEvaluator; override; See also Ancestor Method
```

Overrides [GetEvaluator](../BoldElements/TBoldElement.md#getevaluator).

### GetReverseDeriveMethodForMember

```delphi
function GetReverseDeriveMethodForMember(Member: TBoldMember): TBoldReverseDerive; virtual;
```

Overrides GetDisplayName. The display name for a Bold object is the name of the class.

### GetStringRepresentation

```delphi
function GetStringRepresentation(Representation: TBoldRepresentation): string; override; See also Ancestor Method
```

In the default implementation for a Bold object, the string representation is the attribute with index `Representation`. If that member is not an attribute the `StringRepresentation` is the object's id and classname.

### InternalCreateNewWithClassAndSystem

```delphi
constructor InternalCreateNewWithClassAndSystem(ClassTypeInfo: TBoldClassTypeInfo; aSystem: TBoldSystem; Persistent: Boolean);
```

Bold-internal constructor. Use the normal constructor [Create](TBoldObject.md#create).

### Invalidate

```delphi
procedure Invalidate;
```

All persistent members of the BoldObject will have their values invalidated. Thus, they will be refetched from the persistent storage the next time they are accessed. See [Invalidate](TBoldMember.md#invalidate).

### IsEqualAs

```delphi
function IsEqualAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Boolean; override; See also Ancestor Method
```

Bold objects are compared with reference semantics, meaning that an object is only equal to itself. An object can also be compared to a [TBoldObjectReference](TBoldObjectReference.md) and they will be considered equal if the object reference refers the the object.

### MarkObjectDirty

```delphi
procedure MarkObjectDirty;
```

Forces the object to become dirty. This does not change the value of any of the members of the object.

### MayDelete

```delphi
function MayDelete: Boolean; virtual;
```

Override this function and return `false` to prohibit the delete state transition.

**See Also**

- [CanDelete](TBoldObject.md#candelete)
- StartDelete

### MayUpdate

```delphi
function MayUpdate: Boolean; virtual;
```

Override this function and return `false` to prohibit the update state transition.

**See Also**

- [CanUpdate](TBoldObject.md#canupdate)
- StartUpdate

### PrepareDelete

```delphi
procedure PrepareDelete; virtual;
```

Override this method to add behaviour before an object is deleted. The default implementation is empty.

### PrepareUpdate

```delphi
procedure PrepareUpdate; virtual;
```

Override this method to add behaviour before an object is updated. The default implementation is empty.

### ProxyInterface

```delphi
function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override; See also Ancestor Method
```

Overrides ProxyInterface

### ReceiveEventFromOwned

```delphi
procedure ReceiveEventFromOwned(originator: TObject; originalEvent: TBoldEvent); override; See also Ancestor Method
```

Override this method in one of your business classes to catch any event sent by a member. The member sending the event is the originator.

| **Note** |
|---|

| Don't forget to call `**inherited**`. |
|---|

### ReRead

```delphi
procedure ReRead;
```

The object will be fetched even if its persistence state is current.

### StateError

```delphi
procedure StateError(S: String); override; See also Ancestor Method
```

Overrides [StateError](../BoldDomainElement/TBoldDomainElement.md#stateerror).

### SubscribeToStringRepresentation

```delphi
procedure SubscribeToStringRepresentation(Representation: TBoldRepresentation; Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override; See also Ancestor Method
```

Overrides [SubscribeToStringRepresentation](../BoldElements/TBoldElement.md#subscribetostringrepresentation).

**See Also**

- Subscriptions

### ToBeRemovedClassAccessed

```delphi
procedure ToBeRemovedClassAccessed; virtual;
```

Called when a class with EvolutionState ToBeRemoved is accessed. See Related Topics.

It can be useful during development to place a breakpoint in the implementation of this method to pinpoint when the class is accessed. To do this, add the method to your root class, add the method as override and generate code. The implementation on `TBoldObject` is empty.

### ToBeRemovedMemberAccessed

```delphi
procedure ToBeRemovedMemberAccessed(MemberRTInfo: TBoldMemberRTInfo); virtual;
```

Called when a member with EvolutionState ToBeRemoved is accessed. See Related Topics.

It can be useful during development to place a breakpoint in the implementation of this method to pinpoint when the class is accessed. To do this, add the method to your root class, add the method as override and generate code. The implementation on `TBoldObject` is empty.

### ToBeRemovedMemberModified

```delphi
procedure ToBeRemovedMemberModified(MemberRTInfo: TBoldMemberRTInfo); virtual;
```

Called when a member with EvolutionState ToBeRemoved is accessed. See Related Topics.

It can be useful during development to place a breakpoint in the implementation of this method to pinpoint when the class is accessed. To do this, add the method to your root class, add the method as override and generate code. The implementation on `TBoldObject` is empty.

### UnLinkAll

```delphi
procedure UnLinkAll;
```

The members that correspond to roles are cleared, i.e. members of type `TBoldObjectList` will be emptied and members of type `TBoldObjectReference` will be set to `**nil**`. Associations with association classes will not be removed when `UnlinkAll` is called on a link object.

### ValidateMember

```delphi
function ValidateMember(const ObjectDelphiName, MemberDelphiName: String; GeneratedMemberIndex: integer; MemberClass: TBoldMemberClass): Boolean;
```

Internal convenience method, used by the generated code to verify that it is in sync with the model.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
