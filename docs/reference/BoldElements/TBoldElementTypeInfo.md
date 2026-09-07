# TBoldElementTypeInfo

**Unit**: [BoldElements](index.md)

## Declaration

```delphi
TBoldElementTypeInfo = class(TBoldMetaElement)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldFlaggedObject`
4. [TBoldSubscribableObject](../BoldSubscription/TBoldSubscribableObject.md)
5. [TBoldElement](TBoldElement.md)
6. [TBoldMetaElement](TBoldMetaElement.md)
7. TBoldElementTypeInfo
8. **Direct subclasses**
9. `TBoldAttributeTypeInfo`
10. `TBoldElementTypeInfoWithConstraint`
11. `TBoldListTypeInfo`
12. `TBoldTypeTypeInfo`

## Description

This is the superclass for all classes in the type-herarchy of a BoldSystem.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [BoldValueType](#boldvaluetype) |  | read-only |
| [SystemTypeInfo](#systemtypeinfo) |  | read-only |

### BoldValueType

```delphi
property BoldValueType: TBoldValueTypeSet;
```

### SystemTypeInfo

```delphi
property SystemTypeInfo: TBoldElementTypeInfo;
```

## Methods

| Name | Summary | Notes |
|---|---|---|
| [ConformsTo](#conformsto) |  | abstract |
| [Create](#create) |  |  |
| [GetAsList](#getaslist) |  | override |
| [GetEvaluator](#getevaluator) |  | protected, override |
| [SetValueType](#setvaluetype) |  | protected |

### ConformsTo

```delphi
function ConformsTo(Element: TBoldElementTypeInfo): Boolean; virtual; abstract;
```

### Create

```delphi
constructor Create(const ModelName: string; const ExpressionName: string; const DelphiName: string; SystemTypeInfo: TBoldElementTypeInfo);
```

### GetAsList

```delphi
procedure GetAsList(ResultList: TBoldIndirectElement); override; See also Ancestor Method
```

### GetEvaluator

```delphi
function GetEvaluator: TBoldEvaluator; override; See also Ancestor Method
```

### SetValueType

```delphi
procedure SetValueType(NewValue: TBoldValueTypeSet);
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
