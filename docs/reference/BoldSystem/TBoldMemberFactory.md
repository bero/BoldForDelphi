# TBoldMemberFactory

Creates instances of TBoldMember subclasses

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldMemberFactory = class(TBoldMemoryManagedObject)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. TBoldMemberFactory

## Description

This factory simplifies creating instances of the `TBoldMember` subclasses with a correct type in the bold type system.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [CreateMemberFromBoldType](#creatememberfromboldtype) | Creates a TBoldMember of the desired type |  |
| [CreateMemberFromExpressionName](#creatememberfromexpressionname) | Creates a TBoldMember of the desired type |  |

### CreateMemberFromBoldType

```delphi
class function CreateMemberFromBoldType(BoldType: TBoldElementTypeInfo): TBoldMember;
```

Call this with a `TBoldElementTypeInfo` and it will return an instance of a corresponding type with the `BoldType`-property correctly set.

### CreateMemberFromExpressionName

```delphi
class function CreateMemberFromExpressionName(SystemTypeInfo: TBoldSystemTypeInfo; const Name: String): TBoldMember;
```

Similar to [CreateMemberFromBoldType](TBoldMemberFactory.md#creatememberfromboldtype), but takes a string with the expression name of the type instead.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
