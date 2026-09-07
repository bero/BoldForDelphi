# TBoldAbstractOptimisticLockHandler

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldAbstractOptimisticLockHandler = class(TBoldSystemExtension)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldInterfacedObject`
4. `TBoldNonRefCountedObject`
5. [TBoldSystemExtension](TBoldSystemExtension.md)
6. TBoldAbstractOptimisticLockHandler
7. **Direct subclasses**
8. `TBoldOptimisticLockHandler`

## Properties

| Name | Summary | Notes |
|---|---|---|
| [OldValues](#oldvalues) |  | read-only |

### OldValues

```delphi
property OldValues: IBoldValueSpace;
```

## Methods

| Name | Summary | Notes |
|---|---|---|
| [AddOptimisticLocks](#addoptimisticlocks) |  | abstract |
| [EnsureEnclosure](#ensureenclosure) |  | abstract |

### AddOptimisticLocks

```delphi
procedure AddOptimisticLocks(ObjectList: TBoldObjectlist; PreCondition: TBoldOptimisticLockingPrecondition); virtual; abstract;
```

### EnsureEnclosure

```delphi
procedure EnsureEnclosure(Obj: TBoldObject; Enclosure: TBoldObjectList; ValidateOnly: Boolean; var ListIsEnclosure: Boolean); virtual; abstract;
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
