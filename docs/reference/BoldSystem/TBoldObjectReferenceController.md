# TBoldObjectReferenceController

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldObjectReferenceController = class(TBoldAbstractObjectReferenceController)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. [TBoldAbstractController](TBoldAbstractController.md)
4. [TBoldAbstractObjectReferenceController](TBoldAbstractObjectReferenceController.md)
5. TBoldObjectReferenceController

## Methods

| Name | Summary | Notes |
|---|---|---|
| [AssignContentValue](#assigncontentvalue) |  |  |
| [Create](#create) |  | override |
| [Destroy](#destroy) |  | override |
| [GetLocator](#getlocator) |  | override |
| [GetStreamName](#getstreamname) |  | protected, override |
| [MakeDbCurrent](#makedbcurrent) |  | override |
| [ProxyClass](#proxyclass) |  | protected, override |
| [SetLocator](#setlocator) |  | override |

### AssignContentValue

```delphi
procedure AssignContentValue(Source: IBoldValue);
```

### Create

```delphi
constructor Create(Owner: TBoldObjectReference); override; See also Ancestor Method
```

### Destroy

```delphi
destructor Destroy; override;
```

### GetLocator

```delphi
function GetLocator: TBoldObjectLocator; override; See also Ancestor Method
```

### GetStreamName

```delphi
function GetStreamName: string; override; See also Ancestor Method
```

### MakeDbCurrent

```delphi
procedure MakeDbCurrent; override; See also Ancestor Method
```

### ProxyClass

```delphi
function ProxyClass: TBoldMember_ProxyClass; override; See also Ancestor Method
```

### SetLocator

```delphi
procedure SetLocator(NewLocator: TBoldObjectLocator); override; See also Ancestor Method
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
