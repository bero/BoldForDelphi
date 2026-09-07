# TBoldAbstractObjectReferenceController

Bold-internal class that implements the behaviour of single links.

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldAbstractObjectReferenceController = class(TBoldAbstractController)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. [TBoldAbstractController](TBoldAbstractController.md)
4. TBoldAbstractObjectReferenceController
5. **Direct subclasses**
6. `TBoldDirectSingleLinkController`
7. `TBoldIndirectSingleLinkController`
8. `TBoldLinkObjectReferenceController`
9. [TBoldObjectReferenceController](TBoldObjectReferenceController.md)

## Description

Bold-internal class that implements the behaviour of single links.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [OwningReference](#owningreference) |  | read-only |

### OwningReference

```delphi
property OwningReference: TBoldObjectReference;
```

## Methods

| Name | Summary | Notes |
|---|---|---|
| [ControllerForLinkRole](#controllerforlinkrole) |  | protected |
| [ControllerForMainRole](#controllerformainrole) |  | protected |
| [Create](#create) |  | virtual |
| [GetLocator](#getlocator) |  | abstract |
| [GetOwningMember](#getowningmember) |  | protected, override |
| [MakeDbCurrent](#makedbcurrent) |  | abstract |
| [MayUpdate](#mayupdate) |  | protected, virtual |
| [OtherEndControllerForLinkObject](#otherendcontrollerforlinkobject) |  | protected |
| [PreDiscard](#prediscard) |  | protected, virtual |
| [ProxyClass](#proxyclass) |  | protected, abstract |
| [SetLocator](#setlocator) |  | abstract |

### ControllerForLinkRole

```delphi
function ControllerForLinkRole: TBoldAbstractObjectReferenceController;
```

### ControllerForMainRole

```delphi
function ControllerForMainRole: TBoldAbstractObjectReferenceController;
```

### Create

```delphi
constructor Create(Owner: TBoldObjectReference); virtual;
```

### GetLocator

```delphi
function GetLocator: TBoldObjectLocator; virtual; abstract;
```

### GetOwningMember

```delphi
function GetOwningMember: TBoldMember; override; See also Ancestor Method
```

### MakeDbCurrent

```delphi
procedure MakeDbCurrent; virtual; abstract;
```

### MayUpdate

```delphi
function MayUpdate: Boolean; virtual;
```

### OtherEndControllerForLinkObject

```delphi
function OtherEndControllerForLinkObject(Obj: TBoldObject): TBoldAbstractObjectReferenceController;
```

### PreDiscard

```delphi
procedure PreDiscard; virtual;
```

### ProxyClass

```delphi
function ProxyClass: TBoldMember_ProxyClass; virtual; abstract;
```

### SetLocator

```delphi
procedure SetLocator(NewLocator: TBoldObjectLocator); virtual; abstract;
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
