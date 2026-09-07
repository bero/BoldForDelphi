# TBoldAbstractOldValueHandler

Bold-internal

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldAbstractOldValueHandler = class(TBoldSystemExtension)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldInterfacedObject`
4. `TBoldNonRefCountedObject`
5. [TBoldSystemExtension](TBoldSystemExtension.md)
6. TBoldAbstractOldValueHandler
7. **Direct subclasses**
8. [TBoldAbstractTransActionHandler](TBoldAbstractTransActionHandler.md)
9. `TBoldOldValueHandler`

## Description

Bold-internal

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
| [CopyMemberToValueSpace](#copymembertovaluespace) |  | protected |
| [CopyObjectToValueSpace](#copyobjecttovaluespace) |  | protected |
| [GetOldValues](#getoldvalues) |  | protected, abstract |
| [MemberPersistenceStatePreChange](#memberpersistencestateprechange) |  | abstract |
| [MemberValuePreChange](#membervalueprechange) |  | abstract |
| [NewValueInValueSpace](#newvalueinvaluespace) |  | protected |
| [ObjectExistenceChange](#objectexistencechange) |  | abstract |
| [ObjectExistencePersistenceStateChange](#objectexistencepersistencestatechange) |  | abstract |

### CopyMemberToValueSpace

```delphi
class procedure CopyMemberToValueSpace(BoldMember: TBoldMember; ValueSpace: IBoldValueSpace);
```

### CopyObjectToValueSpace

```delphi
class procedure CopyObjectToValueSpace(BoldObject: TBoldObject; ValueSpace: IBoldValueSpace);
```

### GetOldValues

```delphi
function GetOldValues: IBoldValueSpace; virtual; abstract;
```

### MemberPersistenceStatePreChange

```delphi
procedure MemberPersistenceStatePreChange(BoldMember: TBoldMember; NewState: TBoldValuePersistenceState); virtual; abstract;
```

### MemberValuePreChange

```delphi
procedure MemberValuePreChange(BoldMember: TBoldMember); virtual; abstract;
```

### NewValueInValueSpace

```delphi
class function NewValueInValueSpace(BoldMember: TBoldMember; ValueSpace: IBoldValueSpace): IBoldValue;
```

### ObjectExistenceChange

```delphi
procedure ObjectExistenceChange(BoldObject: TBoldObject); virtual; abstract;
```

### ObjectExistencePersistenceStateChange

```delphi
procedure ObjectExistencePersistenceStateChange(BoldObject: TBoldObject; NewState: TBoldValuePersistenceState); virtual; abstract;
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
