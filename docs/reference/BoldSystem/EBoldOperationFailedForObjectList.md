# EBoldOperationFailedForObjectList

Exception that holds a list of objects

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
EBoldOperationFailedForObjectList = class(EBold)
```

## Hierarchy

1. Exception
2. `EBold`
3. EBoldOperationFailedForObjectList

## Description

This exception is currently only raised when the optimistic locking fails. The objects that failed are in the ObjectList property.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [ObjectList](#objectlist) | Objects that caused the failure | read-only |

### ObjectList

```delphi
property ObjectList: TBoldObjectList;
```

This object list contains the objects that caused the operation to fail.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Create](#create) | Constructor |  |
| [Destroy](#destroy) | Destructor | override |

### Create

```delphi
constructor Create(const msg: string; args: array of const; IdList: TBoldObjectIdList; System: TBoldSystem);
```

Constructor

### Destroy

```delphi
destructor Destroy; override;
```

Destructor

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
