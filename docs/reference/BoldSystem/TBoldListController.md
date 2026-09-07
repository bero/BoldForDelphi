# TBoldListController

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldListController = class(TBoldAbstractController)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. [TBoldAbstractController](TBoldAbstractController.md)
4. TBoldListController
5. **Direct subclasses**
6. [TBoldAbstractObjectListController](TBoldAbstractObjectListController.md)

## Properties

| Name | Summary | Notes |
|---|---|---|
| [BoldSystem](#boldsystem) |  | read-only |
| [CanCreateNew](#cancreatenew) |  | read-only |
| [Count](#count) |  | read-only |
| [OwningList](#owninglist) |  | protected, read-only |

### BoldSystem

```delphi
property BoldSystem: TBoldSystem;
```

### CanCreateNew

```delphi
property CanCreateNew: Boolean;
```

### Count

```delphi
property Count: integer;
```

### OwningList

```delphi
property OwningList: TBoldList;
```

## Methods

| Name | Summary | Notes |
|---|---|---|
| [AddElement](#addelement) |  | abstract |
| [Create](#create) |  |  |
| [CreateNew](#createnew) |  | protected, virtual |
| [GetCanCreateNew](#getcancreatenew) |  | protected, virtual |
| [GetCount](#getcount) |  | protected, abstract |
| [GetElement](#getelement) |  | abstract |
| [GetOwningMember](#getowningmember) |  | protected, override |
| [GetStringrepresentation](#getstringrepresentation) |  | protected, virtual |
| [IncludesElement](#includeselement) |  | abstract |
| [IndexOfElement](#indexofelement) |  | abstract |
| [InsertElement](#insertelement) |  | abstract |
| [Move](#move) |  | abstract |
| [RemoveByIndex](#removebyindex) |  | abstract |
| [SetElement](#setelement) |  | abstract |

### AddElement

```delphi
procedure AddElement(Element: TBoldElement); virtual; abstract;
```

### Create

```delphi
constructor Create(OwningList: TBoldList);
```

### CreateNew

```delphi
function CreateNew: TBoldElement; virtual;
```

### GetCanCreateNew

```delphi
function GetCanCreateNew: Boolean; virtual;
```

### GetCount

```delphi
function GetCount: Integer; virtual; abstract;
```

### GetElement

```delphi
function GetElement(index: Integer): TBoldElement; virtual; abstract;
```

### GetOwningMember

```delphi
function GetOwningMember: TBoldMember; override; See also Ancestor Method
```

### GetStringrepresentation

```delphi
function GetStringrepresentation: String; virtual;
```

### IncludesElement

```delphi
function IncludesElement(Item: TBoldElement): Boolean; virtual; abstract;
```

### IndexOfElement

```delphi
function IndexOfElement(Item: TBoldElement): Integer; virtual; abstract;
```

### InsertElement

```delphi
procedure InsertElement(index: Integer; Element: TBoldElement); virtual; abstract;
```

### Move

```delphi
procedure Move(CurrentIndex: Integer; NewIndex: Integer); virtual; abstract;
```

### RemoveByIndex

```delphi
procedure RemoveByIndex(index: Integer); virtual; abstract;
```

### SetElement

```delphi
procedure SetElement(index: Integer; Value: TBoldElement); virtual; abstract;
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
