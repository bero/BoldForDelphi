# TBoldList

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldList = class(TBoldMember)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldFlaggedObject`
4. [TBoldSubscribableObject](../BoldSubscription/TBoldSubscribableObject.md)
5. [TBoldElement](../BoldElements/TBoldElement.md)
6. [TBoldDomainElement](../BoldDomainElement/TBoldDomainElement.md)
7. [TBoldMember](TBoldMember.md)
8. TBoldList
9. **Direct subclasses**
10. `TBoldElementList`
11. [TBoldMemberList](TBoldMemberList.md)
12. [TBoldObjectList](TBoldObjectList.md)
13. `TBoldTypeList`

## Properties

| Name | Summary | Notes |
|---|---|---|
| [CanCreateNew](#cancreatenew) | If CreateNew is allowed | read-only |
| [Count](#count) | Number of elements in the list. | read-only |
| [DuplicateMode](#duplicatemode) | When an element that is already in the list is added, DuplicateMode determines how the list will behave. |  |
| [Elements](#elements) |  |  |
| [ListController](#listcontroller) |  | protected |

### CanCreateNew

```delphi
property CanCreateNew: Boolean;
```

Returns `True` if [CreateNew](TBoldList.md#createnew) is allowed. Reasons for it not being allowed includes: The type of the elements is unknown, or abstract; the list is read-only.

### Count

```delphi
property Count: Integer;
```

Number of elements in the list.

### DuplicateMode

```delphi
property DuplicateMode: TBoldListDupMode;
```

When an element that is already in the list is added, `DuplicateMode` determines how the list will behave.

- `bldmAllow` - The list allows elements to appear in the list more than once
- `bldmMerge` - Nothing will happen when adding an element that is already in the list
- `bldmError` - It is an error to add an element that is already in the list

### Elements

```delphi
property Elements[index:Integer]: TBoldElement;
```

The default property of the list.

### ListController

```delphi
property ListController: TBoldListController;
```

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Add](#add) | Adds an element to the list. |  |
| [AddElement](#addelement) | Internal method for adding an element to the list, implemented by subclasses. | protected, abstract |
| [AddList](#addlist) | Adds all the elements in List to self. | virtual |
| [AddNew](#addnew) | Creates an adds an element to the list. |  |
| [AddToStrings](#addtostrings) | Adds the contents of the list to a TStrings. |  |
| [AllocateData](#allocatedata) | Bold-internal | protected, virtual |
| [CanClear](#canclear) | Indicates if it is allowed to clear the list. |  |
| [CanInsert](#caninsert) | Indicates if inserting the element is allowed. | virtual |
| [CanMove](#canmove) | Indicates f moving an element is allowed. | virtual |
| [CanRemove](#canremove) | Indicates if removing an element is allowed. | virtual |
| [CanSet](#canset) | Indicates if replacing the element at a given position is allowed. | virtual |
| [Clear](#clear) | Empties the list |  |
| [CreateNew](#createnew) | Bold-internal | protected, virtual |
| [DefaultSubscribe](#defaultsubscribe) | Overrides TBoldElement.DefaultSubscribe. | override |
| [Destroy](#destroy) | Destructor | override |
| [DuplicateControl](#duplicatecontrol) | Bold-internal convenience method | protected |
| [EnsureCanCreateNew](#ensurecancreatenew) | Bold-internal convenience method | protected |
| [EnsureRange](#ensurerange) | Optimized EnsureCurrent | virtual |
| [FreeData](#freedata) | Bold-internal | protected, abstract |
| [GetAsList](#getaslist) | Overrides TBoldElement.GetAsList | override |
| [GetCanCreateNew](#getcancreatenew) |  | protected, virtual |
| [GetCount](#getcount) | Get-method for the Count property. | protected, abstract |
| [GetElement](#getelement) | Get-method for the Element property | protected, abstract |
| [GetStringRepresentation](#getstringrepresentation) | Overrides TBoldElement.GetStringRepresentation | protected, override |
| [Includes](#includes) | True if Element is in the list |  |
| [IncludesElement](#includeselement) | Internal method for testing if an element is in the list, implmented by subclasses. | protected, abstract |
| [IndexOf](#indexof) | Return the position of Item in the list. |  |
| [IndexOfElement](#indexofelement) | Internal method for getting the index of an element in the list, implemented by subclasses. | protected, abstract |
| [InitializeMember](#initializemember) | Overrides TBoldMember.InitializeMember | protected, override |
| [Insert](#insert) | Inserts element in a given position in the list. |  |
| [InsertElement](#insertelement) | Internal method for inserting an element, implemented by subclasses. | protected, abstract |
| [InsertNew](#insertnew) | Creates a new element and inserts it into the list | abstract |
| [InternalAddNew](#internaladdnew) | Internal method for adding a new element to the list, implemented by subclasses. | protected, abstract |
| [InternalClear](#internalclear) |  | protected, abstract |
| [MakeContentsImmutable](#makecontentsimmutable) |  |  |
| [Move](#move) | Changes the position of an element in the list. | abstract |
| [Remove](#remove) | Removes the element from the list | virtual |
| [RemoveByIndex](#removebyindex) | Removes the element at a given position from the list. | abstract |
| [SetElement](#setelement) | Set-method for the Elements property. | protected, abstract |
| [Sort](#sort) | Sorts the list |  |
| [ToStrings](#tostrings) | Makes the TStrings S contain the elements in the list and their string representations Representation. |  |
| [ToStringsWithNil](#tostringswithnil) | Same as ToStrings, except that it adds a nil element, with NilString, as the first element in S . |  |

### Add

```delphi
procedure Add(Element: TBoldElement);
```

Elements will be added at the end of the list

### AddElement

```delphi
procedure AddElement(Element: TBoldElement); virtual; abstract;
```

Internal method for adding an element to the list, implemented by subclasses.

### AddList

```delphi
procedure AddList(List: TBoldList); virtual;
```

Adds all the elements in `List` to self.

If the list is [immutable](../BoldElements/TBoldElement.md#mutable), the call will fail with an exception.

### AddNew

```delphi
function AddNew: TBoldElement;
```

If the list allows adding new elements, an element of the list's type will be created and added to the list.

If adding an element is not allowed an exception and a `BoldFailure` will be raised.

Reasons for failure includes that the list is [immutable](../BoldElements/TBoldElement.md#mutable) or the type of the requested element is abstract.

### AddToStrings

```delphi
procedure AddToStrings(Representation: TBoldRepresentation; S: TStrings);
```

Will add the elements and their string representations to the `TStrings S`.

### AllocateData

```delphi
procedure AllocateData; virtual;
```

Bold-internal

### CanClear

```delphi
function CanClear(Subscriber: TBoldSubscriber): Boolean;
```

You can make `MayClear` return `false`, and thus prohibit `Clear` on a `TBoldList`, by subscribing to the bqMayClear query and returning `false`.

### CanInsert

```delphi
function CanInsert(index: Integer; Element: TBoldElement; Subscriber: TBoldSubscriber): Boolean; virtual;
```

Returns `True` if `Element` may be inserted in position `**index**` in the list. A programmer can disallow [Insert](TBoldList.md#insert) by subscribing to the bqMayInsert query. If a `Subscriber` is passed to the function, subscriptions will be placed to notify when the result of `MayInsert` may have changed.

### CanMove

```delphi
function CanMove(CurIndex, NewIndex: Integer; Subscriber: TBoldSubscriber = nil): Boolean; virtual;
```

Returns `True` if the element in position `CurIndex` may be moved to position `NewIndex` in the list. A programmer can disallow [Move](TBoldList.md#move) by subscribing to the bqMayMove query. If a `Subscriber` is passed to the function, subscriptions will be placed to notify when the result of `MayMove` may have changed.

### CanRemove

```delphi
function CanRemove(index: Integer; Subscriber: TBoldSubscriber): Boolean; virtual;
```

Returns `True` if the element in position `**index**` may be removed from the list. A programmer can disallow [Remove](TBoldList.md#remove) by subscribing to the bqMayRemove query. If a `Subscriber` is passed to the function, subscriptions will be placed to notify when the result of `MayRemove` may have changed.

### CanSet

```delphi
function CanSet(index: Integer; Item: TBoldElement; Subscriber: TBoldSubscriber): Boolean; virtual;
```

Returns `True` if the element in position `**index**` in the list may be replaced by `Item`. This corresponds to assigning to the [Elements](TBoldList.md#elements) array property. A programmer can disallow this by subscribing to the bqMayReplace query. If a `Subscriber` is passed to the function, subscriptions will be placed to notify when the result of `MayReplace` may have changed.

### Clear

```delphi
procedure Clear;
```

Clear removes all elements from the list.

### CreateNew

```delphi
function CreateNew: TBoldElement; virtual;
```

Bold-internal. Used by [AddNew](TBoldList.md#addnew).

### DefaultSubscribe

```delphi
procedure DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override; See also Ancestor Method
```

`DefaultSubscribe` on `TBoldList` will subscribe to the events `beItemAdded`, `beItemDeleted`, `beItemReplaced` and `beOrderChanged`.

**See Also**

- Subscriptions

### Destroy

```delphi
destructor Destroy; override; See also Ancestor Method
```

Destructor

### DuplicateControl

```delphi
function DuplicateControl: Boolean;
```

Bold-internal convenience method

### EnsureCanCreateNew

```delphi
procedure EnsureCanCreateNew;
```

Bold-internal convenience method

### EnsureRange

```delphi
procedure EnsureRange(FromIndex: integer; ToIndex: integer); virtual;
```

If some of the elements in the range are invalid, they will be made current. This is efficient, as it will fetch all objects at once, rather than one at a time.

### FreeData

```delphi
procedure FreeData; virtual; abstract;
```

Bold-internal

### GetAsList

```delphi
procedure GetAsList(ResultList: TBoldIndirectElement); override; See also Ancestor Method
```

ResultList's value will be the list.

### GetCanCreateNew

```delphi
function GetCanCreateNew: Boolean; virtual;
```

private

### GetCount

```delphi
function GetCount: Integer; virtual; abstract;
```

Get-method for the [Count](TBoldList.md#count) property.

### GetElement

```delphi
function GetElement(index: Integer): TBoldElement; virtual; abstract;
```

Get-method for the `Element` property. Implemented by subclasses.

### GetStringRepresentation

```delphi
function GetStringRepresentation(Representation: TBoldRepresentation): string; override; See also Ancestor Method
```

Overrides [GetStringRepresentation](../BoldElements/TBoldElement.md#getstringrepresentation).

### Includes

```delphi
function Includes(Item: TBoldElement): Boolean;
```

This function returns `true` if `Element`is a member of the list. `Includes` may be considerably faster than checking [IndexOf](TBoldList.md#indexof)`<> -1` as it can make smarter use of existing indexes.

### IncludesElement

```delphi
function IncludesElement(Item: TBoldElement): Boolean; virtual; abstract;
```

Internal method for testing if an element is in the list, implmented by subclasses.

### IndexOf

```delphi
function IndexOf(Item: TBoldElement): Integer;
```

Return the position of `Item` in the list, or `-1` if `item` is not in the list. If there are multiple occurences of `Item`, `IndexOf` will return the index of the first occurence.

### IndexOfElement

```delphi
function IndexOfElement(Item: TBoldElement): Integer; virtual; abstract;
```

Internal method for getting the index of an element in the list, implemented by subclasses.

### InitializeMember

```delphi
procedure InitializeMember(AOwningElement: TBoldDomainElement; ElementTypeInfo: TBoldElementTypeInfo); override; See also Ancestor Method
```

Overrides [InitializeMember](TBoldMember.md#initializemember)

### Insert

```delphi
procedure Insert(index: Integer; Element: TBoldElement);
```

Inserts `element` in a position `**index**`in the list.

### InsertElement

```delphi
procedure InsertElement(index: Integer; Element: TBoldElement); virtual; abstract;
```

Internal method for inserting an element, implemented by subclasses.

### InsertNew

```delphi
procedure InsertNew(index: Integer); virtual; abstract;
```

If the list type's element type is a concrete type, an element of this type will be created and inserted in the list.

### InternalAddNew

```delphi
function InternalAddNew: TBoldElement; virtual; abstract;
```

Internal method for adding a new element to the list, implemented by subclasses.

### InternalClear

```delphi
procedure InternalClear; virtual; abstract;
```

### MakeContentsImmutable

```delphi
procedure MakeContentsImmutable;
```

### Move

```delphi
procedure Move(CurIndex, NewIndex: Integer); virtual; abstract;
```

Moves the element with index `CurIndex` to the position `NewIndex` in the list. Elements between `CurIndex` and `NewIndex` will have their index shifted by one.

### Remove

```delphi
procedure Remove(Item: TBoldElement); virtual;
```

`Element` is removed from the list. If `Element` is not a member of the list an exception is raised.

### RemoveByIndex

```delphi
procedure RemoveByIndex(index: Integer); virtual; abstract;
```

Removes the element at position `**Index**` from the list. If `**Index**` is outside the list the result is undefined.

### SetElement

```delphi
procedure SetElement(index: Integer; Value: TBoldElement); virtual; abstract;
```

Set-method for the `Elements` property. Implemented by subclasses.

### Sort

```delphi
procedure Sort(CompareFunc: TBoldElementCompare);
```

Sorts the elements in the list according to `CompareFunc`.

| **Note** |
|---|

| For an ordered association, this actually changes the internal order between the objects. So, for display purposes, make a copy of the list and sort and display the copy instead. |
|---|

### ToStrings

```delphi
procedure ToStrings(Representation: TBoldRepresentation; S: TStrings);
```

Makes the TStrings `S` contain the elements in the list and their string representations `Representation`. The difference between this method and [AddToStrings](TBoldList.md#addtostrings) is that it will remove any existing data in `S`.

### ToStringsWithNil

```delphi
procedure ToStringsWithNil(Representation: TBoldRepresentation; S: TStrings; nilString: string);
```

Same as [ToStrings](TBoldList.md#tostrings), except that it adds a nil element, with `NilString`, as the first element in `S`.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
