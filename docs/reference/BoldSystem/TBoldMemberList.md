# TBoldMemberList

A list containing elements of type TBoldMember.

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldMemberList = class(TBoldList)
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
9. TBoldMemberList

## Description

A list containing elements of type `TBoldMember`.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [BoldMembers](#boldmembers) | Property for accessing the contents of the list. |  |
| [CloneMembers](#clonemembers) | If the list owns the members |  |

### BoldMembers

```delphi
property BoldMembers[index:Integer]: TBoldMember;
```

Property for accessing the contents of the list. If the property is written to, and [CloneMembers](TBoldMemberList.md#clonemembers) is `true`, a copy of the member will be used instead, and the previous member in position `**index**` will be freed.

### CloneMembers

```delphi
property CloneMembers: Boolean;
```

If `CloneMembers` is `true` members will be cloned when added to the list. The clones will be owned by the list, and thus destroyed when removed from the list or when the list is destroyed. If `CloneMembers` is `false` the list will simply contain references to the members. The value of `CloneMembers` can only be changed if the list is empty.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Add](#add) | Adds a member to the list. |  |
| [AddElement](#addelement) | Overrides TBoldList.AddElement | protected, override |
| [AllocateData](#allocatedata) | Overrides TBoldList.AllocateData. | protected, override |
| [Assign](#assign) | Overrides TBoldElement.Assign. | override |
| [CreateNew](#createnew) | Overrides TBoldList.CreateNew. | protected, override |
| [FreeData](#freedata) | Overrides TBoldList.CreateNew. | protected, override |
| [GetCanCreateNew](#getcancreatenew) | Overrides TBoldList.GetCanCreateNew. | protected, override |
| [GetCount](#getcount) | Overrides TBoldList.GetCount. | protected, override |
| [GetElement](#getelement) | Overrides TBoldList.GetElement. | protected, override |
| [GetStreamName](#getstreamname) | Overrides TBoldMember.GetStreamName. | protected, override |
| [GetStringRepresentation](#getstringrepresentation) | Overrides TBoldElement.GetStringRepresentation. | protected, override |
| [IncludesElement](#includeselement) | Overrides TBoldList.IncludesElement. | protected, override |
| [IndexOf](#indexof) | Returns the position of the first occurence of Item in the list. |  |
| [IndexOfElement](#indexofelement) | Overrides TBoldList.IndexOfElement. | protected, override |
| [InitializeMember](#initializemember) | Bold-internal | protected, override |
| [Insert](#insert) | Inserts an element in a given position in the list. |  |
| [InsertElement](#insertelement) | Overrides TBoldList.InsertElement. | protected, override |
| [InsertNew](#insertnew) | Overrides TBoldList.InsertNew. | override |
| [InternalAddNew](#internaladdnew) | Overrides TBoldList.InternalAddNew. | protected, override |
| [InternalClear](#internalclear) |  | protected, override |
| [Move](#move) | Overrides TBoldList.Move. | override |
| [ProxyClass](#proxyclass) | Overrides TBoldMember.ProxyClass. | protected, override |
| [RemoveByIndex](#removebyindex) | Overrides TBoldList.RemoveByIndex | override |
| [SetElement](#setelement) | Overrides TBoldList.SetElement. | protected, override |

### Add

```delphi
procedure Add(Item: TBoldMember);
```

Adds a member to the list. If [CloneMembers](TBoldMemberList.md#clonemembers) is `true` a copy of the member will be added.

### AddElement

```delphi
procedure AddElement(Element: TBoldElement); override; See also Ancestor Method
```

Overrides [AddElement](TBoldList.md#addelement)

### AllocateData

```delphi
procedure AllocateData; override; See also Ancestor Method
```

Overrides [AllocateData](TBoldList.md#allocatedata).

### Assign

```delphi
procedure Assign(Source: TBoldElement); override; See also Ancestor Method
```

Overrides [Assign](../BoldElements/TBoldElement.md#assign). If [CloneMembers](TBoldMemberList.md#clonemembers) is true copies of the members in `source` will be added to the list, otherwise the same members will be added.

### CreateNew

```delphi
function CreateNew: TBoldElement; override; See also Ancestor Method
```

Overrides [CreateNew](TBoldList.md#createnew).

### FreeData

```delphi
procedure FreeData; override; See also Ancestor Method
```

Overrides [CreateNew](TBoldList.md#createnew).

### GetCanCreateNew

```delphi
function GetCanCreateNew: Boolean; override; See also Ancestor Method
```

Overrides [GetCanCreateNew](TBoldList.md#getcancreatenew).

### GetCount

```delphi
function GetCount: Integer; override; See also Ancestor Method
```

Overrides [GetCount](TBoldList.md#getcount).

### GetElement

```delphi
function GetElement(index: Integer): TBoldElement; override; See also Ancestor Method
```

Overrides [GetElement](TBoldList.md#getelement).

### GetStreamName

```delphi
function GetStreamName: String; override; See also Ancestor Method
```

Overrides TBoldMember.GetStreamName. Streaming is not implemented for `TBoldMemberList`.

### GetStringRepresentation

```delphi
function GetStringRepresentation(Representation: TBoldRepresentation): string; override; See also Ancestor Method
```

Overrides [GetStringRepresentation](../BoldElements/TBoldElement.md#getstringrepresentation). The string representation of a `TBoldMemberList` is the count.

### IncludesElement

```delphi
function IncludesElement(Item: TBoldElement): Boolean; override; See also Ancestor Method
```

Overrides [IncludesElement](TBoldList.md#includeselement).

### IndexOf

```delphi
function IndexOf(Item: TBoldMember): Integer;
```

Returns the position of the first occurence of `Item` in the list.

### IndexOfElement

```delphi
function IndexOfElement(Item: TBoldElement): Integer; override; See also Ancestor Method
```

Overrides [IndexOfElement](TBoldList.md#indexofelement).

### InitializeMember

```delphi
procedure InitializeMember(AOwningElement: TBoldDomainElement; ElementTypeInfo: TBoldElementTypeInfo); override; See also Ancestor Method
```

Bold-Internal

### Insert

```delphi
procedure Insert(index: Integer; Item: TBoldMember);
```

Inserts `Item` in position `**index**` in the list. If [CloneMembers](TBoldMemberList.md#clonemembers) is `true`, a copy of the member will be inserted instead.

### InsertElement

```delphi
procedure InsertElement(index: Integer; Element: TBoldElement); override; See also Ancestor Method
```

Overrides [InsertElement](TBoldList.md#insertelement).

### InsertNew

```delphi
procedure InsertNew(index: Integer); override; See also Ancestor Method
```

Overrides [InsertNew](TBoldList.md#insertnew).

### InternalAddNew

```delphi
function InternalAddNew: TBoldElement; override; See also Ancestor Method
```

Overrides [InternalAddNew](TBoldList.md#internaladdnew)

### InternalClear

```delphi
procedure InternalClear; override; See also Ancestor Method
```

### Move

```delphi
procedure Move(CurIndex, NewIndex: Integer); override; See also Ancestor Method
```

Overrides [Move](TBoldList.md#move).

### ProxyClass

```delphi
function ProxyClass: TBoldMember_ProxyClass; override; See also Ancestor Method
```

Overrides [ProxyClass](TBoldMember.md#proxyclass).

### RemoveByIndex

```delphi
procedure RemoveByIndex(index: Integer); override; See also Ancestor Method
```

Overrides TBoldList.RemoveByIndex. If [CloneMembers](TBoldMemberList.md#clonemembers) is `true`, the member will not only be removed but also freed.

### SetElement

```delphi
procedure SetElement(index: Integer; Value: TBoldElement); override; See also Ancestor Method
```

Overrides [SetElement](TBoldList.md#setelement).

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
