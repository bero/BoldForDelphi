# TBoldAbstractObjectListController

Bold-internal class that implements the behaviour of object lists.

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldAbstractObjectListController = class(TBoldListController)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. [TBoldAbstractController](TBoldAbstractController.md)
4. [TBoldListController](TBoldListController.md)
5. TBoldAbstractObjectListController
6. **Direct subclasses**
7. `TBoldLinkObjectListController`
8. `TBoldMultiLinkController`
9. `TBoldObjectListController`

## Description

Bold-internal class that implements the behaviour of object lists.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [OwningObjectList](#owningobjectlist) |  | protected, read-only |

### OwningObjectList

```delphi
property OwningObjectList: TBoldObjectList;
```

## Methods

| Name | Summary | Notes |
|---|---|---|
| [AddElement](#addelement) |  | override |
| [AddLocator](#addlocator) |  | abstract |
| [AtTime](#attime) |  | virtual |
| [Clear](#clear) |  | virtual |
| [FreeContent](#freecontent) |  | virtual |
| [GetElement](#getelement) |  | override |
| [GetLocator](#getlocator) |  | abstract |
| [GetLocatorByQualifiersAndSubscribe](#getlocatorbyqualifiersandsubscribe) |  | abstract |
| [GetObjectList](#getobjectlist) |  | protected |
| [HandlesAtTime](#handlesattime) |  | virtual |
| [IncludesElement](#includeselement) |  | override |
| [IncludesLocator](#includeslocator) |  | abstract |
| [IndexOfElement](#indexofelement) |  | override |
| [IndexOfLocator](#indexoflocator) |  | abstract |
| [InsertElement](#insertelement) |  | override |
| [InsertLocator](#insertlocator) |  | abstract |
| [MakeDbCurrent](#makedbcurrent) |  | abstract |
| [PrepareClear](#prepareclear) |  | protected, virtual |
| [ProxyClass](#proxyclass) |  | protected, abstract |
| [SetElement](#setelement) |  | override |
| [SetLocator](#setlocator) |  | abstract |

### AddElement

```delphi
procedure AddElement(Element: TBoldElement); override; See also Ancestor Method
```

### AddLocator

```delphi
procedure AddLocator(Element: TBoldObjectLocator); virtual; abstract;
```

### AtTime

```delphi
function AtTime(Time: TBoldTimestampType): TBoldMember; virtual;
```

### Clear

```delphi
procedure Clear; virtual;
```

### FreeContent

```delphi
procedure FreeContent; virtual;
```

### GetElement

```delphi
function GetElement(index: Integer): TBoldElement; override; See also Ancestor Method
```

### GetLocator

```delphi
function GetLocator(index: Integer): TBoldObjectLocator; virtual; abstract;
```

### GetLocatorByQualifiersAndSubscribe

```delphi
function GetLocatorByQualifiersAndSubscribe(MemberList: TBoldMemberList; Subscriber: TBoldSubscriber): TBoldObjectLocator; virtual; abstract;
```

### GetObjectList

```delphi
function GetObjectList: TBoldObjectList;
```

### HandlesAtTime

```delphi
function HandlesAtTime: Boolean; virtual;
```

### IncludesElement

```delphi
function IncludesElement(Item: TBoldElement): Boolean; override; See also Ancestor Method
```

### IncludesLocator

```delphi
function IncludesLocator(Item: TBoldObjectLocator): Boolean; virtual; abstract;
```

### IndexOfElement

```delphi
function IndexOfElement(Item: TBoldElement): Integer; override; See also Ancestor Method
```

### IndexOfLocator

```delphi
function IndexOfLocator(Item: TBoldObjectLocator): Integer; virtual; abstract;
```

### InsertElement

```delphi
procedure InsertElement(index: Integer; Element: TBoldElement); override; See also Ancestor Method
```

### InsertLocator

```delphi
procedure InsertLocator(index: Integer; Element: TBoldObjectLocator); virtual; abstract;
```

### MakeDbCurrent

```delphi
procedure MakeDbCurrent; virtual; abstract;
```

### PrepareClear

```delphi
procedure PrepareClear; virtual;
```

### ProxyClass

```delphi
function ProxyClass: TBoldMember_ProxyClass; virtual; abstract;
```

### SetElement

```delphi
procedure SetElement(index: Integer; Value: TBoldElement); override; See also Ancestor Method
```

### SetLocator

```delphi
procedure SetLocator(index: Integer; Value: TBoldObjectLocator); virtual; abstract;
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
