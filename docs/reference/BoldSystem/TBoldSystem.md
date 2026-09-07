# TBoldSystem

The implementation of an object space

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldSystem = class(TBoldDomainElement)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldFlaggedObject`
4. [TBoldSubscribableObject](../BoldSubscription/TBoldSubscribableObject.md)
5. [TBoldElement](../BoldElements/TBoldElement.md)
6. [TBoldDomainElement](../BoldDomainElement/TBoldDomainElement.md)
7. TBoldSystem

## Description

This is the implementation of an object space. It holds all object instances for a model.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [AsIBoldvalueSpace](#asiboldvaluespace) | The value space view of the system | read-only |
| [BoldSystemTypeInfo](#boldsystemtypeinfo) | The run-time model for the system | read-only |
| [ClassByExpressionName](#classbyexpressionname) | Gets the list of all instances of a given class. | read-only |
| [Classes](#classes) | The lists of all instances of the classes | read-only |
| [DirtyObjects](#dirtyobjects) | All objects that have changed in memory | read-only |
| [EnsuredLocatorByID](#ensuredlocatorbyid) | The TBoldObjectLocator for the Bold object corresponding to ObjectID | read-only |
| [IsDefault](#isdefault) | If the system is the default system |  |
| [Locators](#locators) | All loaded locators in the system | read-only |
| [NewDirtyList](#newdirtylist) | List that catches modified objects |  |
| [NewModifiedList](#newmodifiedlist) | List that catches modified objects |  |
| [OnPreUpdate](#onpreupdate) |  |  |
| [OptimisticLockHandler](#optimisticlockhandler) | Bold-internal | read-only |
| [PersistenceController](#persistencecontroller) | The persistence controller of the system | read-only |
| [PessimisticLockHandler](#pessimisticlockhandler) | Used for pessimistic locking |  |
| [RegionFactory](#regionfactory) | Bold-internal | read-only |
| [SystemPersistenceHandler](#systempersistencehandler) | For internal use | protected, read-only |
| [TimeForTimestamp](#timefortimestamp) | When a specified time stamp occurred. | read-only |
| [TimestampForTime](#timestampfortime) | What time stamp occurred at a specified time. | read-only |
| [TimeStampOfLatestUpdate](#timestampoflatestupdate) | The time stamp associated with the latest update operation performed on the system. | read-only |
| [TransactionMode](#transactionmode) | How the system uses transactions internally |  |
| [UndoHandler](#undohandler) | Bold-internal | read-only |
| [UndoHandlerInterface](#undohandlerinterface) | The interface to Undo/Redo functionality of the system | read-only |

### AsIBoldvalueSpace

```delphi
property AsIBoldvalueSpace[Mode:TBoldDomainElementProxyMode]: IBoldvalueSpace;
```

Use this property to get an `IBoldValueSpace` interface to the system. This is used internally.

### BoldSystemTypeInfo

```delphi
property BoldSystemTypeInfo: TBoldSystemTypeInfo;
```

The run-time model for the system

### ClassByExpressionName

```delphi
property ClassByExpressionName[constExpressionName:string]: TBoldObjectList;
```

Gets the list of all instances of a given class.

### Classes

```delphi
property Classes[index:Integer]: TBoldObjectList;
```

Gets the list of all instances of a class with a given topsorted index.

### DirtyObjects

```delphi
property DirtyObjects: TList;
```

This list contains all objects, for this system, that have changed in memory, and needs to be updated in the persistent storage.

### EnsuredLocatorByID

```delphi
property EnsuredLocatorByID[ObjectID:TBoldObjectId]: TBoldObjectLocator;
```

The [TBoldObjectLocator](TBoldObjectLocator.md) for the Bold object corresponding to ObjectID. If none exists, the system creates one.

### IsDefault

```delphi
property IsDefault: Boolean;
```

The default system is the system returned by [DefaultSystem](TBoldSystem.md#defaultsystem). Calling [MakeDefault](TBoldSystem.md#makedefault) makes a system the default system.

### Locators

```delphi
property Locators: TBoldSystemLocatorList;
```

This list contains all the locators that have been fetched, even if their objects have not.

### NewDirtyList

```delphi
property NewDirtyList: TBoldObjectList;
```

This property is typically assigned by the user from the `OnActivate` event of a form. The list will then contain all objects that have been made dirty by that form. Note that this is different from the [NewModifiedList](TBoldSystem.md#newmodifiedlist) in that objects are added only when they first become dirty.

### NewModifiedList

```delphi
property NewModifiedList: TBoldObjectList;
```

This property is typically assigned by the user from the `OnActivate` event of a form. The list will then contain all objects that have been modfied by that form. Note that this is different from the [NewDirtyList](TBoldSystem.md#newdirtylist) in that objects are added even if they were already dirty.

### OnPreUpdate

```delphi
property OnPreUpdate: TNotifyEvent;
```

### OptimisticLockHandler

```delphi
property OptimisticLockHandler: TBoldAbstractOptimisticLockHandler;
```

Bold-internal

### PersistenceController

```delphi
property PersistenceController: TBoldPersistenceController;
```

The persistence controller of the system. If `PersistenceController` is `**nil**` the system is transient.

### PessimisticLockHandler

```delphi
property PessimisticLockHandler: TBoldAbstractPessimisticLockHandler;
```

If assigned, the elements of the system will request locks pessimistically before being modified. Do not set the property directly. Instead use a `TBoldLockingHandle`, and connect it to the [TBoldSystemHandle](../BoldSystemHandle/TBoldSystemHandle.md).

### RegionFactory

```delphi
property RegionFactory: TBoldAbstractRegionFactory;
```

Bold-internal

### SystemPersistenceHandler

```delphi
property SystemPersistenceHandler: TBoldAbstractSystemPersistenceHandler;
```

For internal use

### TimeForTimestamp

```delphi
property TimeForTimestamp[Timestamp:TBoldTimestampType]: TDateTime;
```

When a specified time stamp occurred.

| **Note** |
|---|

| Note! This feature is only available in the Object Versioning Extension to Bold for Delphi/Bold for C++. |
|---|

### TimestampForTime

```delphi
property TimestampForTime[ClockTime:TDateTime]: TBoldTimestampType;
```

What time stamp occurred at a specified time.

| **Note** |
|---|

| This feature is only available in the Object Versioning Extension to Bold for Delphi/Bold for C++. |
|---|

### TimeStampOfLatestUpdate

```delphi
property TimeStampOfLatestUpdate: TBoldTimeStampType;
```

The time stamp associated with the latest update operation performed on the system. The value is -1 if no update operation has been performed yet.

| **Note** |
|---|

| This feature is only available in the Object Versioning Extension to Bold for Delphi/Bold for C++. |
|---|

### TransactionMode

```delphi
property TransactionMode: TBoldSystemTransactionMode;
```

The system uses transactions internally for various operations, such as cascading delete and manipulating associations. It is possible to optimize perfomance by setting this property to `stmUnsafe`. This will cause the system not to use transactions. However, should any operation fail while in unsafe mode, the system becomes potentially unstable and inconsistent.

### UndoHandler

```delphi
property UndoHandler: TBoldAbstractUndoHandler;
```

Bold-internal

### UndoHandlerInterface

```delphi
property UndoHandlerInterface: IBoldUndoHandler;
```

This interface allows the user to perform Undo/Redo of changes in the system.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [AllowObjectDestruction](#allowobjectdestruction) | Allows Bold objects to be destroyed. |  |
| [AssertLinkIntegrity](#assertlinkintegrity) | Bold-internal |  |
| [CommitTransaction](#committransaction) | Commits an in-memory transaction |  |
| [Create](#create) | Constructor | override |
| [CreateExistingObjectByID](#createexistingobjectbyid) | Bold-internal |  |
| [CreateNewObjectByExpressionName](#createnewobjectbyexpressionname) | Creates a new Bold object |  |
| [CreateWithTypeInfo](#createwithtypeinfo) | Creates a TBoldSystem instance for a given model. |  |
| [DefaultSubscribe](#defaultsubscribe) | Overrides TBoldElement.DefaultSubscribe | override |
| [DefaultSystem](#defaultsystem) | The global default Bold system, if there is one. |  |
| [DelayObjectDestruction](#delayobjectdestruction) | Stops Bold objects from being destroyed. |  |
| [Destroy](#destroy) | Destructor | override |
| [Discard](#discard) | Discards all dirty objects in the Bold system. |  |
| [EnsureCanDestroy](#ensurecandestroy) |  |  |
| [EnsureEnclosure](#ensureenclosure) | Makes a list technically consistent for updating. |  |
| [FetchLinksWithObjects](#fetchlinkswithobjects) | Optimized fetching of links |  |
| [GetAllInClass](#getallinclass) | All instances of the specified class will be added to the list. |  |
| [GetAllInClassWithSQL](#getallinclasswithsql) | Fills aList with all the instances of AClass that match the SQL condition WhereClause. |  |
| [GetAllWithCondition](#getallwithcondition) | Fills List with all instances that matches the condition. |  |
| [GetAsList](#getaslist) | Overrides TBoldElement.GetAsList | override |
| [GetBoldDirty](#getbolddirty) | Overrides TBoldDomainElement.GetBoldDirty | protected, override |
| [GetBoldType](#getboldtype) | Overrides TBoldElement.GetBoldType | protected, override |
| [GetDisplayName](#getdisplayname) | Overrides TBoldElement.GetDisplayName | protected, override |
| [GetEvaluator](#getevaluator) | Overrides TBoldElement.GetEvaluator | protected, override |
| [GetStringRepresentation](#getstringrepresentation) | Overrides TBoldElement.GetStringRepresentation | protected, override |
| [InTransaction](#intransaction) | If the system is currently in a transaction |  |
| [MakeDefault](#makedefault) | Makes the system the default system |  |
| [ProxyInterface](#proxyinterface) | Overrides TBoldElement.ProxyInterface | override |
| [ReceiveEventFromOwned](#receiveeventfromowned) | Overrides TBoldDomainElement.ReceiveEventFromOwned | protected, override |
| [RollbackTransaction](#rollbacktransaction) | Undoes all changes made in the system since the transaction was started. |  |
| [StartTransaction](#starttransaction) | Starts an in-memory transaction |  |
| [TryCommitTransaction](#trycommittransaction) | Commit that doesn't raise exceptions |  |
| [UpdateDatabase](#updatedatabase) | Write changes to persistent storage |  |
| [UpdateDatabaseWithList](#updatedatabasewithlist) | Write selected objects to persistent storage. |  |

### AllowObjectDestruction

```delphi
procedure AllowObjectDestruction;
```

Allows Bold objects to be destroyed. This may result in the destruction of objects whose destruction was delayed. See [DelayObjectDestruction](TBoldSystem.md#delayobjectdestruction) for details.

### AssertLinkIntegrity

```delphi
function AssertLinkIntegrity: Boolean;
```

Bold-internal

### CommitTransaction

```delphi
procedure CommitTransaction(MinimalMode: TBoldSystemTransactionMode = stmNormal);
```

`TBoldSystem.CommitTransaction` ends a transaction started with [TBoldSystem.StartTransaction](TBoldSystem.md#starttransaction). If the transaction is not allowed to commit an exception is raised. There are two ways this could happen.

First, if there have been nested transactions, and one of the inner transactions failed (i.e., [RollbackTransaction](TBoldSystem.md#rollbacktransaction) was called), then the entire transaction must fail, and thus `CommitTransaction` will raise an exception.

Second, there are a couple of places where validation rules can be inserted. There is a virtual method [MayCommit](../BoldDomainElement/TBoldDomainElement.md#maycommit) on [TBoldDomainElement](../BoldDomainElement/TBoldDomainElement.md), and an associated query-event `bqMayCommit`. These are called on each Bold object and Bold member that have been modified during the transaction. The system itself also sends the `bqMayCommit` query-event. If any of these return `False` then the transaction is not allowed to commit and `CommitTransaction` raises an exception. If there are nested transactions the validation is only performed on the outmost nesting level.

There is an alternative method [TBoldSystem.TryCommitTransaction](TBoldSystem.md#trycommittransaction) that indicates its success by returning `true` if the transaction could commit, or `false` if it failed. It also automatically performs a rollback if the transaction failed.

### Create

```delphi
constructor Create(AOwningElement: TBoldDomainElement); override; See also Ancestor Method
```

This constructor cannot be used to create a TBoldSystem. Instead, use [CreateWithTypeInfo](TBoldSystem.md#createwithtypeinfo).

### CreateExistingObjectByID

```delphi
function CreateExistingObjectByID(BoldObjectID: TBoldObjectId): TBoldObject;
```

This function is used internally by Bold to create the in-memory representation of a Bold object. It does not fetch the object from the persistence layer, and all members are initialized to Invalid. For normal Bold users, there is no need to call this function directly.

### CreateNewObjectByExpressionName

```delphi
function CreateNewObjectByExpressionName(const ExpressionName: string; Persistent: Boolean = True): TBoldObject;
```

This method is useful for creating new Bold objects if there is no generated code. If there is generated code, the instance created will be of the correct Delphi type. Without generated code, the instance will be an instance of [TBoldObject](TBoldObject.md).

### CreateWithTypeInfo

```delphi
constructor CreateWithTypeInfo(AOwningElement: TBoldDomainElement; SystemTypeInfo: TBoldSystemTypeInfo; PersistenceController: TBoldPersistenceController; RegionFactory: TBoldAbstractRegionFactory = nil);
```

This constructor creates a `TBoldSystem` instance for a given model. For normal Bold users, there is no need to call this function. Use a [TBoldSystemHandle](../BoldSystemHandle/TBoldSystemHandle.md) component instead.

### DefaultSubscribe

```delphi
procedure DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override; See also Ancestor Method
```

No subscriptions are placed on a system.

**See Also**

- Subscriptions

### DefaultSystem

```delphi
class function DefaultSystem: TBoldSystem;
```

If there exists (globally in the application) a Bold system with IsDefault = true, that system will be returned.

### DelayObjectDestruction

```delphi
procedure DelayObjectDestruction;
```

After this method is called, no instances of [TBoldObject](TBoldObject.md) or its descendants will be destroyed until a subsequent call to [AllowObjectDestruction](TBoldSystem.md#allowobjectdestruction) is made. Objects that were supposed to be destroyed during this time will be queued and destoyed later. If several calls to `DelayObjectDestruction` are made, no objects will be destroyed until the last matching call to `AllowObjectDestruction` is made.

### Destroy

```delphi
destructor Destroy; override; See also Ancestor Method
```

Destructor

### Discard

```delphi
procedure Discard;
```

Discards all dirty objects in the Bold system. This method also discards all transient objects.

### EnsureCanDestroy

```delphi
procedure EnsureCanDestroy;
```

private

### EnsureEnclosure

```delphi
function EnsureEnclosure(ObjectList: TBoldObjectList; ValidateOnly: Boolean): Boolean;
```

`EnsureEnclosure` takes a list of objects and adds those additional objects needed to make it technically consistent for updating. For example, if an object in the list has an association to a newly created object, the new object will also be added to the list, so that the database does not contain a broken association.

`EnsureEnclosure` returns `true` if the list was already complete, and false if objects were added to make it complete.

If `ValidateOnly` is true the list will not actually be altered, but only the result value will be computed.

### FetchLinksWithObjects

```delphi
procedure FetchLinksWithObjects(ObjectList: TBoldObjectList; const LinkName: string);
```

Use this method to optimize the fetching of objects from the persistent storage. The OCL-evaluator will do this whenever possible. It will ensure that all objects in the list will have the named relation fetched, and all related objects in this association will also be fetched.

### GetAllInClass

```delphi
procedure GetAllInClass(aList: TBoldObjectList; AClass: TBoldObjectClass);
```

All instances of the specified class will be added to the list.

### GetAllInClassWithSQL

```delphi
procedure GetAllInClassWithSQL(aList: TBoldObjectList; AClass: TBoldObjectClass; WhereClause, OrderByClause: String; Params: TParams = nil; JoinInheritedTables: Boolean = true; MaxAnswers: integer = -1; Offset: integer = -1);
```

Fills `aList` with all the instances of `AClass` that match the SQL condition `WhereClause`. The condition will be evaluated in the persistent storage, therefore changes in the system that has not been stored may not be reflected in the result.

An easier way to use this method is to use the `TBoldSQLHandle` component

| Parameters |
|---|

| aList | The list to be filled with the resulting objects |
|---|---|
| aClass | The class from which you want your result such as "`TPerson`" |
| WhereClause | a SQL-fragment that restricts the objects based on their attributes or relations. This can be arbitrarily complex and may contain nested select-statements |
| OrderByClause | a SQL-fragment (optionally empty) that specifies the order of the objects |
| Params | If the SQLfragment contains values such as dates, it is good to send these values as params instead of as text (since many databases rely on the operating system to decide the string format of a date). Give the parameter a name, and refer to the value using ":name" in your `WhereClause` |
| JoinInheritedTables | If the `WhereClause` contains references to inherited attributes or relations, then this property must be `true`, so that the tables from the super classes are joined in the query. This will normally decrease performance |
| MaxAnswer | If you are not sure how many objects will be returned by your query, you can restrict the number in the result set by providing a value for the `MaxAnswer` parameter. |
| Offset | If you want to skip initial objects (perhaps becayuse they have already been retrieved with a previous call to this method with a restriction in `MaxAnswer`) you can provide a value for the `Offset` parameter |

### GetAllWithCondition

```delphi
procedure GetAllWithCondition(aList: TBoldObjectList; Condition: TBoldCondition);
```

Fills `List` with all instances that matches the condition. The condition will be evaluated in the persistent storage, therefore changes in the system that has not been stored may not be reflected in the result.

### GetAsList

```delphi
procedure GetAsList(ResultList: TBoldIndirectElement); override; See also Ancestor Method
```

The list representation of a system is a [TBoldMemberList](TBoldMemberList.md) containing object lists with all instances of all classes in the system.

### GetBoldDirty

```delphi
function GetBoldDirty: Boolean; override; See also Ancestor Method
```

A system is dirty if it contains any dirty objects.

### GetBoldType

```delphi
function GetBoldType: TBoldElementTypeInfo; override; See also Ancestor Method
```

The BoldType of a system is its SystemTypeInfo, i.e. the run-time representation of the model.

### GetDisplayName

```delphi
function GetDisplayName: String; override; See also Ancestor Method
```

The display name of a system is the name of the model.

### GetEvaluator

```delphi
function GetEvaluator: TBoldEvaluator; override; See also Ancestor Method
```

Overrides [GetEvaluator](../BoldElements/TBoldElement.md#getevaluator)

### GetStringRepresentation

```delphi
function GetStringRepresentation(Representation: TBoldRepresentation): string; override; See also Ancestor Method
```

The string representation of a system is the expression name of the model.

### InTransaction

```delphi
function InTransaction: boolean;
```

If there is an open transaction in the system, i.e. if a call to [StartTransaction](TBoldSystem.md#starttransaction) has been made without a matching call to [CommitTransaction](TBoldSystem.md#committransaction) or [RollbackTransaction](TBoldSystem.md#rollbacktransaction).

### MakeDefault

```delphi
procedure MakeDefault;
```

Makes the system the default system. See [DefaultSystem](TBoldSystem.md#defaultsystem).

### ProxyInterface

```delphi
function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override; See also Ancestor Method
```

Overrides ProxyInterface

### ReceiveEventFromOwned

```delphi
procedure ReceiveEventFromOwned(originator: TObject; originalEvent: TBoldEvent); override; See also Ancestor Method
```

Overrides [ReceiveEventFromOwned](../BoldDomainElement/TBoldDomainElement.md#receiveeventfromowned)

### RollbackTransaction

```delphi
procedure RollbackTransaction(MinimalMode: TBoldSystemTransactionMode = stmNormal);
```

Undoes all changes made in the system since the transaction was started (with a call to [StartTransaction](TBoldSystem.md#starttransaction)). If the transaction is nested inside another StartTransaction/CommitTransaction pair, then the transaction is not rolled back straight away. Rather, this happens at the outmost nesting level.

### StartTransaction

```delphi
procedure StartTransaction(MinimalMode: TBoldSystemTransactionMode = stmNormal);
```

A series of object manipulations can be enclosed in a transaction, thus allowing them to either execute as a whole, or be rolled back. A transaction is started with a call to TBoldSystem.StartTransaction. Any subsequent changes to Bold objects in that system becomes part of the transaction. If successful, the transaction is ended with a call to [TBoldSystem.CommitTransaction](TBoldSystem.md#committransaction). If some part of the transaction fails, it can be rolled back with a call to [TBoldSystem.RollbackTransaction](TBoldSystem.md#rollbacktransaction). In that case, all changes that were made since the transaction started are undone.

You can have nested calls to `StartTransaction` and `CommitTransaction`/`RollbackTransaction`. In that case, the entire transaction is either commited or rolled back at the outmost level only. That is, if one of the inner transactions fail, then the entire transaction also fails.

**StartTransaction Example**The following code structure is recommended for transactions:

| aBoldSystem.StartTransaction; **try** ...operations that are part **of** the transaction... aBoldSystem.CommitTransaction; **except** aBoldSystem.RollbackTransaction; **raise**; **end**; |
|---|

### TryCommitTransaction

```delphi
function TryCommitTransaction(MinimalMode: TBoldSystemTransactionMode = stmNormal): Boolean;
```

`TrCommitTransaction` works like [CommitTransaction](TBoldSystem.md#committransaction), but will not raise an exception if the commit fails. Instead, it will rollback the transaction and return false.

### UpdateDatabase

```delphi
procedure UpdateDatabase;
```

Calling this method will cause every dirty object (new objects, deleted objects or objects that have been changed) to be sent to the persistence mechanism (normally a database).

If Optimistic Locking is enabled, and this fails, an `EBoldOperationFailedforObjectList` exception will be raised

**See Also**

- [UpdateDatabaseWithList](TBoldSystem.md#updatedatabasewithlist)
- [Discard](TBoldSystem.md#discard)

### UpdateDatabaseWithList

```delphi
procedure UpdateDatabaseWithList(ObjectList: TBoldObjectList);
```

Calling this method will cause the objects in the objectlist to be sent to the persistence mechanism (normally a database). An enclosure of objects will be added to ensure that no embedded links in the persistent storage point to invalid objects.

It's the `TBoldSystemPersistenceHandler.UpdateDatabaseWithList` that takes care of the actual saving of the objects in the list.

If Optimistic Locking is enabled, and this fails, an `EBoldOperationFailedforObjectList` exception will be raised

**See Also**

- [UpdateDatabase](TBoldSystem.md#updatedatabase)
- [Discard](TBoldSystem.md#discard)

## Events

| Name | Summary | Notes |
|---|---|---|
| [OnCreateApproximateObjectError](#oncreateapproximateobjecterror) | This event is raised when an object with inexact type is created |  |
| [OnOptimisticLockingFailed](#onoptimisticlockingfailed) | Called when UpdateDatabase fails because of an optimisic lock. |  |

### OnCreateApproximateObjectError

```delphi
TBoldCreateApproximateObjectError = procedure(Obj: TBoldObject) of object;
```

When an object is recreated as a part of being fetched, the persistence mechanism is usually able to determine exactly what type the object should be before trying to recreate the object. If it is impossible to determine the exact type of the object (perhaps because the object has been deleted in the persistent storage), the system is unable to create the object. Normally, this will result in an exception. If this event is set, and the approximate type of the object is not an abstract type, the system will allow the creation of the superclass instead, and call this event to notify the application developer that something is wrong.

The most likely scenario when this happens is if an object has an embedded relation to another object, and this related object has subclasses, and the related object has been deleted. If the event is defined, the related object will be recreated in memory and the event will be called. Normally, the persistence mechanism will mark such an object as deleted and readonly.

Running the data integrity validator from the model-editor would normally detect this problem.

### OnOptimisticLockingFailed

```delphi
TBoldOptimisticLockingFailedEvent = procedure(UpdateList, FailureList: TBoldObjectList; const FailureReason: String) of object;
```

Called when UpdateDatabase fails because of an optimisic lock. See Optimistic Locking. If the event is assigned, no exception will be raised.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
