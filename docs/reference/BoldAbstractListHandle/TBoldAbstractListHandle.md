# TBoldAbstractListHandle

Superclass for all list handles.

**Unit**: [BoldAbstractListHandle](index.md)

## Declaration

```delphi
TBoldAbstractListHandle = class(TBoldRootedHandle)
```

## Hierarchy

1. TComponent
2. [TBoldSubscribableComponent](../BoldSubscription/TBoldSubscribableComponent.md)
3. [TBoldElementHandle](../BoldHandles/TBoldElementHandle.md)
4. [TBoldNonSystemHandle](../BoldHandles/TBoldNonSystemHandle.md)
5. [TBoldRootedHandle](../BoldRootedHandles/TBoldRootedHandle.md)
6. TBoldAbstractListHandle
7. **Direct subclasses**
8. `TBoldCursorHandle`

## Description

TBoldAbstractListHandle is an abstract superclass for handles with a cursor-type behaviour. Its purpose is to represent a list of elements, and a position in that list.

It is an extension of [TBoldElementHandle](../BoldHandles/TBoldElementHandle.md) in that it really has two values, the [List](TBoldAbstractListHandle.md#list) property, and the [Value](../BoldHandles/TBoldElementHandle.md#value) property, where this Value property is effectively `List[CurrentIndex]`. There are a number of visual controls, notably `TBoldNavigator`, `TBoldGrid` and `TBoldListBox` that connect to a list handle. They are a bit special compared to other controls in that they not only manipulate values in the Object-Space, but also can set [CurrentIndex](TBoldAbstractListHandle.md#currentindex), which resides in the listhandle itself.

The handle is designed to make Chained Evaluation as natural as possible.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [Count](#count) | Number of elements in the list. | read-only |
| [CurrentBoldObject](#currentboldobject) | CurrentElement as TBoldObject . | read-only |
| [CurrentElement](#currentelement) | Current element in list. | read-only |
| [CurrentIndex](#currentindex) | Current position in list. |  |
| [HasNext](#hasnext) | Use HasNext to determine if at end of list. | read-only |
| [HasPrior](#hasprior) | Indicates whether it is possible to move backward in the list. | read-only |
| [List](#list) | List represented by handle. | read-only |
| [ListElementType](#listelementtype) | Type of elements in List. | read-only |
| [ListType](#listtype) | The "Best Available" type | read-only |
| [MutableList](#mutablelist) | Returns a list that is possible to change. | read-only |
| [MutableObjectList](#mutableobjectlist) | Returns the MutableList as a TBoldObjectList. | read-only |
| [ObjectList](#objectlist) | Returns the List as a TBoldObjectList. | read-only |
| [StaticListType](#staticlisttype) | The list type of the StaticBoldType | read-only |

### Count

```delphi
property Count: Integer;
```

This is mostly equivalent to `List.Count`, with the added provision of being 0 if [List](TBoldAbstractListHandle.md#list) is `**nil**`.

### CurrentBoldObject

```delphi
property CurrentBoldObject: TBoldObject;
```

If [CurrentElement](TBoldAbstractListHandle.md#currentelement) is a `TBoldObject`, then this property will be that value, type cast to a `TBoldObject`, otherwise an exception will be raised.

### CurrentElement

```delphi
property CurrentElement: TBoldElement;
```

`CurrentElement` returns the element that is current in the list. This will be the same as the value property of the handle

### CurrentIndex

```delphi
property CurrentIndex: Integer;
```

This property can be read to find the current index of the listhandle, or set programmatically to change the current index in the list.

**See Also**

- [CurrentElement](TBoldAbstractListHandle.md#currentelement)
- [CurrentBoldObject](TBoldAbstractListHandle.md#currentboldobject)

### HasNext

```delphi
property HasNext: Boolean;
```

`HasNext` is a concept similar to `Eof`, but returning `true` while there is a next element.

Example:

| aListHandle.First; **while** aListHandle.HasNext **do** aListHandle.Next; |
|---|

### HasPrior

```delphi
property HasPrior: Boolean;
```

This is true if the [CurrentIndex](TBoldAbstractListHandle.md#currentindex) property is greater than 0.

### List

```delphi
property List: TBoldList;
```

List represented by handle.

### ListElementType

```delphi
property ListElementType: TBoldElementTypeInfo;
```

This is the type of the elements of the list.

| **Note** |
|---|

| This is subtly different from BoldType. It will be equivalent to `BoldType` when (`Value <> **nil**`) or (`List = **nil**`). However if there is list, but (`Value = **nil**`), either because the list is empty, or (`CurrentIndex = -1`), `ListElementType` will be the type of the elements in that list, rather than `StaticBoldType`. |
|---|

### ListType

```delphi
property ListType: TBoldListTypeInfo;
```

The type of the list, if available. [StaticListType](TBoldAbstractListHandle.md#staticlisttype), otherwise.

### MutableList

```delphi
property MutableList: TBoldList;
```

`MutableObjectList` will return a list that is possible to change. If the [List](TBoldAbstractListHandle.md#list) is mutable, it will be used.

### MutableObjectList

```delphi
property MutableObjectList: TBoldObjectList;
```

`MutableObjectList` will determine if the [MutableList](TBoldAbstractListHandle.md#mutablelist) is a [TBoldObjectList](../BoldSystem/TBoldObjectList.md). If so the the list will be returned as such, otherwise `**nil**` will be returned

### ObjectList

```delphi
property ObjectList: TBoldObjectList;
```

`ObjectList` will determine if the `List` is a [TBoldObjectList](../BoldSystem/TBoldObjectList.md). If so, the list will be returned as such, otherwise `**nil**` will be returned.

### StaticListType

```delphi
property StaticListType: TBoldListTypeInfo;
```

The list type of the [StaticBoldType](../BoldHandles/TBoldElementHandle.md#staticboldtype)

## Methods

| Name | Summary | Notes |
|---|---|---|
| [First](#first) | Sets CurrentIndex to 0. |  |
| [GetCurrentElement](#getcurrentelement) | Get-method for the CurrentElement property | protected, abstract |
| [GetCurrentIndex](#getcurrentindex) | Get-method for the CurrentIndex property | protected, abstract |
| [GetList](#getlist) | Get-method for the List property | protected, abstract |
| [GetMutableList](#getmutablelist) | Get-method for the MutableList property | protected, virtual |
| [Last](#last) | Set CurrentIndex to Count - 1. |  |
| [Next](#next) | Move forward in list |  |
| [Prior](#prior) | Move backwards in the list |  |
| [RemoveCurrentElement](#removecurrentelement) | Remove current element from list |  |
| [SetCurrentIndex](#setcurrentindex) | Set-method for the CurrentIndex property | protected, abstract |

### First

```delphi
procedure First;
```

Sets [CurrentIndex](TBoldAbstractListHandle.md#currentindex) to 0.

### GetCurrentElement

```delphi
function GetCurrentElement: TBoldElement; virtual; abstract;
```

Get-method for the [CurrentElement](TBoldAbstractListHandle.md#currentelement) property

### GetCurrentIndex

```delphi
function GetCurrentIndex: Integer; virtual; abstract;
```

Get-method for the [CurrentIndex](TBoldAbstractListHandle.md#currentindex) property

### GetList

```delphi
function GetList: TBoldList; virtual; abstract;
```

Get-method for the [List](TBoldAbstractListHandle.md#list) property

### GetMutableList

```delphi
function GetMutableList: TBoldList; virtual;
```

Get-method for the [MutableList](TBoldAbstractListHandle.md#mutablelist) property

### Last

```delphi
procedure Last;
```

Set [CurrentIndex](TBoldAbstractListHandle.md#currentindex) to [Count](TBoldAbstractListHandle.md#count) - 1.

### Next

```delphi
procedure Next;
```

Increases [CurrentIndex](TBoldAbstractListHandle.md#currentindex) if it is less than [Count](TBoldAbstractListHandle.md#count).

### Prior

```delphi
procedure Prior;
```

Decrease [CurrentIndex](TBoldAbstractListHandle.md#currentindex) if it is more than 0.

### RemoveCurrentElement

```delphi
procedure RemoveCurrentElement;
```

Remove the [CurrentElement](TBoldAbstractListHandle.md#currentelement) from the [List](TBoldAbstractListHandle.md#list). If the list is a class-list (such as `Person.allInstances`), this will result in the object being deleted.

### SetCurrentIndex

```delphi
procedure SetCurrentIndex(Value: Integer); virtual; abstract;
```

Set-method for the [CurrentIndex](TBoldAbstractListHandle.md#currentindex) property

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
