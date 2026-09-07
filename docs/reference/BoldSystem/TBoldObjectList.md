# TBoldObjectList

A list of Bold objects

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldObjectList = class(TBoldList)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldFlaggedObject`
4. [TBoldSubscribableObject](../BoldSubscription/TBoldSubscribableObject.md)
5. [TBoldElement](../BoldElements/TBoldElement.md)
6. [TBoldDomainElement](../BoldDomainElement/TBoldDomainElement.md)
7. [TBoldMember](TBoldMember.md)
8. [TBoldList](TBoldList.md)
9. TBoldObjectList
10. **Direct subclasses**
11. `TBoldAbstractDirtyList`
12. `TBusinessClassesRootList`

## Description

Apart from being a list of Bold objects, `TBoldObjectList` also offers methods for manipulating the list as a list of [TBoldObjectLocator](TBoldObjectLocator.md)s. Using these methods can be more efficient, as the objects won't have to be fetched from persistent storage.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [Adjusted](#adjusted) | Bold-internal |  |
| [BoldObjects](#boldobjects) | The objects in the list |  |
| [BoldRoleRTInfo](#boldrolertinfo) |  | read-only |
| [Locators](#locators) | The locators of the objects in the list. |  |
| [SubscribeToLocatorsInList](#subscribetolocatorsinlist) | If the list should detect, and automatically remove, locators that are freed. |  |
| [SubscribeToObjectsInList](#subscribetoobjectsinlist) | If the list should automatically remove objects that are deleted. |  |

### Adjusted

```delphi
property Adjusted: Boolean;
```

Bold-internal

### BoldObjects

```delphi
property BoldObjects[index:Integer]: TBoldObject;
```

The objects in the list

### BoldRoleRTInfo

```delphi
property BoldRoleRTInfo: TBoldRoleRTInfo;
```

This property is the same as [BoldMemberRtInfo](TBoldMember.md#boldmemberrtinfo), casted to `TBoldRoleRTInfo`.

### Locators

```delphi
property Locators[index:Integer]: TBoldObjectLocator;
```

The locators of the objects in the list.

### SubscribeToLocatorsInList

```delphi
property SubscribeToLocatorsInList: Boolean;
```

When this property is `false`, the list will not detect if any of its locators are destroyed. This is only useful if you know that it will never happen (otherwise you will get dangling references, and loads of problems).

**See Also**

- Subscriptions

### SubscribeToObjectsInList

```delphi
property SubscribeToObjectsInList: Boolean;
```

Set this property to `False` if you need a list that can hold deleted objects, or if the automatic removal upsets your looping. Default is `True`.

**See Also**

- Subscriptions

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Add](#add) | Adds a Bold object to the list |  |
| [AddElement](#addelement) | Overrides TBoldList.AddElement | protected, override |
| [AddList](#addlist) | Overrides TBoldList.AddList | override |
| [AddLocator](#addlocator) | Adds an object by using its locator. |  |
| [AllocateData](#allocatedata) | Overrides TBoldList.AllocateData | protected, override |
| [Assign](#assign) | Overrides TBoldElement.Assign | override |
| [AtTime](#attime) | Overrides TBoldMember.AtTime | override |
| [CanInsert](#caninsert) | Overrides TBoldList.CanInsert | override |
| [CanInsertLocator](#caninsertlocator) | Same as CanInsert, but takes a locator instead |  |
| [CanSet](#canset) | Overrides TBoldList.CanSet | override |
| [CanSetLocator](#cansetlocator) | Same as CanSet, but takes a locator instead |  |
| [CreateObjectIdList](#createobjectidlist) | Creates an object id list, containing the object ids of the objects in the list. |  |
| [CreateTypedList](#createtypedlist) | Obsolete |  |
| [EnsureObjects](#ensureobjects) | Fetches several objects in one operation. |  |
| [EnsureRange](#ensurerange) | Fetches several objects in one operation. | override |
| [FillFromIDList](#fillfromidlist) | Adds the objects with the ids in IdList. |  |
| [FreeContent](#freecontent) | Overrides TBoldMember.FreeContent | protected, override |
| [FreeData](#freedata) | Overrides TBoldList.FreeData | protected, override |
| [GetByIndex](#getbyindex) | Lookup using qualifiers |  |
| [GetByIndexAndSubscribe](#getbyindexandsubscribe) | Same as GetByIndex, but also places subscriptions |  |
| [GetCount](#getcount) | Overrides TBoldList.GetCount | protected, override |
| [GetElement](#getelement) | Overrides TBoldList.GetElement | protected, override |
| [GetStreamName](#getstreamname) | Overrides TBoldMember.GetStreamName | protected, override |
| [Includes](#includes) | Same as TBoldList.Includes, but typed as TBoldObject. |  |
| [IncludesElement](#includeselement) | Overrides TBoldList.IncludesElement. | protected, override |
| [IndexOf](#indexof) | Same as TBoldList.IndexOf, but typed as TBoldObject |  |
| [IndexOfElement](#indexofelement) | Overrides TBoldList.IndexOfElement. | protected, override |
| [IndexOfLocator](#indexoflocator) | Same as IndexOf, but takes a locator instead. |  |
| [InitializeMember](#initializemember) | Overrides TBoldMember.InitializeMember. | protected, override |
| [Insert](#insert) | Same as TBoldList.Insert, but types as a TBoldObject. |  |
| [InsertElement](#insertelement) | Overrides TBoldList.InsertElement. | protected, override |
| [InsertLocator](#insertlocator) | Same as Insert, but takes a locator instead. |  |
| [InsertNew](#insertnew) | Overrides TBoldList.InsertNew. | override |
| [InternalAddNew](#internaladdnew) | Overrides TBoldList.InternalAddNew. | protected, override |
| [InternalClear](#internalclear) |  | protected, override |
| [InternalCreateClassList](#internalcreateclasslist) | Bold-internal |  |
| [LocatorInList](#locatorinlist) | Indicates if the object with a given locator is in the list. |  |
| [Move](#move) | Overrides TBoldList.Move. | override |
| [ObserverMayModify](#observermaymodify) | Overrides TBoldElement.ObserverMayModify. | override |
| [ProxyClass](#proxyclass) | Overrides TBoldMember.ProxyClass. | protected, override |
| [ProxyInterface](#proxyinterface) | Overrides TBoldElement.ProxyInterface. | override |
| [RemoveByIndex](#removebyindex) | Overrides TBoldList.RemoveByIndex. | override |
| [SetElement](#setelement) | Overrides TBoldList.SetElement | protected, override |

### Add

```delphi
procedure Add(BoldObject: TBoldObject);
```

Same as [Add](TBoldList.md#add), except that the argument is typed as a TBoldObject.

### AddElement

```delphi
procedure AddElement(Element: TBoldElement); override; See also Ancestor Method
```

Overrides [AddElement](TBoldList.md#addelement)

### AddList

```delphi
procedure AddList(List: TBoldList); override; See also Ancestor Method
```

Overrides [AddList](TBoldList.md#addlist)

### AddLocator

```delphi
procedure AddLocator(NewLocator: TBoldObjectLocator);
```

Adds the locator `NewLocator`, and thus indirectly the Bold object associated with the locator, to the list. This may be a useful optimization if you don't actually need to fetch the object's data.

### AllocateData

```delphi
procedure AllocateData; override; See also Ancestor Method
```

Overrides [AllocateData](TBoldList.md#allocatedata)

### Assign

```delphi
procedure Assign(Source: TBoldElement); override; See also Ancestor Method
```

If `Source` is a `TBoldObjectList`, then the objects in `source` will be added to the list. Objects not in `Source` will be removed from the list.

### AtTime

```delphi
function AtTime(Time: TBoldTimestampType): TBoldMember; override; See also Ancestor Method
```

Overrides [AtTime](TBoldMember.md#attime)

### CanInsert

```delphi
function CanInsert(index: Integer; Element: TBoldElement; Subscriber: TBoldSubscriber): Boolean; override; See also Ancestor Method
```

See [CanInsert](TBoldList.md#caninsert)

### CanInsertLocator

```delphi
function CanInsertLocator(index: Integer; Locator: TBoldObjectLocator; Subscriber: TBoldSubscriber): Boolean;
```

Returns `True` if `Locator` may be inserted in position `**index**` in the list. A programmer can disallow `Insert` by subscribing to the `bqMayInsert` query. If a `Subscriber` is passed to the function, subscriptions will be placed to notify when the result of `MayInsert` may have changed.

### CanSet

```delphi
function CanSet(index: Integer; Element: TBoldElement; Subscriber: TBoldSubscriber): Boolean; override; See also Ancestor Method
```

See [CanSet](TBoldList.md#canset)

### CanSetLocator

```delphi
function CanSetLocator(index: Integer; Locator: TBoldObjectLocator; Subscriber: TBoldSubscriber): Boolean;
```

Returns `True` if the locator in position `**index**` in the list may be replaced by `Locator`. This corresponds to assigning to the [Locators](TBoldObjectList.md#locators) array property. A programmer can disallow this by subscribing to the `bqMayReplace` query. If a `Subscriber` is passed to the function, subscriptions will be placed to notify when the result of `MayReplace` may have changed.

### CreateObjectIdList

```delphi
function CreateObjectIdList: TBoldObjectIdList;
```

Creates an object id list, containing the object ids of the objects in the list. Ownership of the id list is passed to the caller.

### CreateTypedList

```delphi
constructor CreateTypedList(ObjectClass: TBoldObjectClass);
```

Obsolete. If you want to create an object list for a specific class, use the generated list type instead (such as `TPersonList`, for the class `TPerson`).

### EnsureObjects

```delphi
procedure EnsureObjects;
```

If some of the objects in the list are not loaded, these will be fetched from persistent storage. This can increase efficiency as all objects are fetched in one operation, rather than one at a time.

### EnsureRange

```delphi
procedure EnsureRange(FromIndex: integer; ToIndex: integer); override; See also Ancestor Method
```

Same as [EnsureObjects](TBoldObjectList.md#ensureobjects), except that only the objects between `FromIndex` and `ToIndex` are fetched.

### FillFromIDList

```delphi
procedure FillFromIDList(ObjectIdList: TBoldObjectIdList; BoldSystem: TBoldSystem);
```

The objects who's object ids are in `IdList` will be added to the list. Objects that are already in the list remain.

### FreeContent

```delphi
procedure FreeContent; override; See also Ancestor Method
```

Overrides [FreeContent](TBoldMember.md#freecontent)

### FreeData

```delphi
procedure FreeData; override; See also Ancestor Method
```

Overrides [FreeData](TBoldList.md#freedata)

### GetByIndex

```delphi
function GetByIndex(MemberList: TBoldMemberList): TBoldObject;
```

If the object list corresponds to a qualified association, this function will return the object that is qualified by the values in the member list. It is usually easier to use the qualified property on the owning object instead such as `aBank.Accounts['anAccountNumber']`.

### GetByIndexAndSubscribe

```delphi
function GetByIndexAndSubscribe(MemberList: TBoldMemberList; Subscriber: TBoldSubscriber): TBoldObject;
```

Same as `GetByIndex`, but also places subscriptions

**See Also**

- Subscriptions

### GetCount

```delphi
function GetCount: Integer; override; See also Ancestor Method
```

Overrides [GetCount](TBoldList.md#getcount)

### GetElement

```delphi
function GetElement(index: Integer): TBoldElement; override; See also Ancestor Method
```

Overrides [GetElement](TBoldList.md#getelement)

### GetStreamName

```delphi
function GetStreamName: String; override; See also Ancestor Method
```

Overrides [GetStreamName](TBoldMember.md#getstreamname)

### Includes

```delphi
function Includes(BoldObject: TBoldObject): Boolean;
```

Same as [Includes](TBoldList.md#includes), but typed as `TBoldObject`.

### IncludesElement

```delphi
function IncludesElement(Item: TBoldElement): Boolean; override; See also Ancestor Method
```

Overrides [IncludesElement](TBoldList.md#includeselement)

### IndexOf

```delphi
function IndexOf(BoldObject: TBoldObject): Integer;
```

Return the position of `BoldObject` in the list, or `-1` if the item is not in the list. If there are multiple occurences of `BoldObject`, `IndexOf` will return the index of the first occurence.

### IndexOfElement

```delphi
function IndexOfElement(Item: TBoldElement): Integer; override; See also Ancestor Method
```

Overrides [IndexOfElement](TBoldList.md#indexofelement).

### IndexOfLocator

```delphi
function IndexOfLocator(Locator: TBoldObjectLocator): Integer;
```

Same as [IndexOf](TBoldObjectList.md#indexof), but takes a locator instead.

### InitializeMember

```delphi
procedure InitializeMember(AOwningElement: TBoldDomainElement; ElementTypeInfo: TBoldElementTypeInfo); override; See also Ancestor Method
```

Overrides [InitializeMember](TBoldMember.md#initializemember).

### Insert

```delphi
procedure Insert(index: Integer; BoldObject: TBoldObject);
```

Same as [Insert](TBoldList.md#insert), but typed as a `TBoldObject`.

### InsertElement

```delphi
procedure InsertElement(index: Integer; Element: TBoldElement); override; See also Ancestor Method
```

Overrides [InsertElement](TBoldList.md#insertelement).

### InsertLocator

```delphi
procedure InsertLocator(index: Integer; Locator: TBoldObjectLocator);
```

Same as [Insert](TBoldObjectList.md#insert), but takes a locator instead.

### InsertNew

```delphi
procedure InsertNew(index: Integer); override; See also Ancestor Method
```

Overrides [InsertNew](TBoldList.md#insertnew).

### InternalAddNew

```delphi
function InternalAddNew: TBoldElement; override; See also Ancestor Method
```

Overrides [InternalAddNew](TBoldList.md#internaladdnew).

### InternalClear

```delphi
procedure InternalClear; override; See also Ancestor Method
```

### InternalCreateClassList

```delphi
constructor InternalCreateClassList(System: TBoldSystem; ListTypeInfo: TBoldListTypeINfo);
```

Bold-internal

### LocatorInList

```delphi
function LocatorInList(NewLocator: TBoldObjectLocator): Boolean;
```

This method is often the fastest way to test if an object is in the list. Since the list is indexed, `LocatorInList` is faster than using [IndexOf](TBoldObjectList.md#indexof). Also, `LocatorInList` does not fetch the object.

### Move

```delphi
procedure Move(CurIndex, NewIndex: Integer); override; See also Ancestor Method
```

Overrides [Move](TBoldList.md#move).

### ObserverMayModify

```delphi
function ObserverMayModify(Observer: TObject): Boolean; override; See also Ancestor Method
```

Overrides [ObserverMayModify](../BoldElements/TBoldElement.md#observermaymodify).

### ProxyClass

```delphi
function ProxyClass: TBoldMember_ProxyClass; override; See also Ancestor Method
```

Overrides [ProxyClass](TBoldMember.md#proxyclass).

### ProxyInterface

```delphi
function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override; See also Ancestor Method
```

Overrides ProxyInterface.

### RemoveByIndex

```delphi
procedure RemoveByIndex(index: Integer); override; See also Ancestor Method
```

Overrides [RemoveByIndex](TBoldList.md#removebyindex).

### SetElement

```delphi
procedure SetElement(index: Integer; Value: TBoldElement); override; See also Ancestor Method
```

Overrides [SetElement](TBoldList.md#setelement)

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
