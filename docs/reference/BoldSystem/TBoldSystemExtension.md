# TBoldSystemExtension

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldSystemExtension = class(TBoldNonRefcountedObject)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldInterfacedObject`
4. `TBoldNonRefCountedObject`
5. TBoldSystemExtension
6. **Direct subclasses**
7. [TBoldAbstractOldValueHandler](TBoldAbstractOldValueHandler.md)
8. [TBoldAbstractOptimisticLockHandler](TBoldAbstractOptimisticLockHandler.md)
9. [TBoldAbstractPessimisticLockHandler](TBoldAbstractPessimisticLockHandler.md)
10. [TBoldAbstractSystemPersistenceHandler](TBoldAbstractSystemPersistenceHandler.md)
11. [TBoldAbstractUndoHandler](TBoldAbstractUndoHandler.md)

## Properties

| Name | Summary | Notes |
|---|---|---|
| [System](#system) |  | read-only |

### System

```delphi
property System: TBoldSystem;
```

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Create](#create) |  | virtual |

### Create

```delphi
constructor Create(System: TBoldSystem); virtual;
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
